#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_dir <- file.path(project_root, "outputs")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

pkgs <- c("arrow", "dplyr", "tidyr", "purrr", "stringr", "stringi", "readr", "tibble", "Matrix", "igraph", "stopwords")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(stringi)
  library(readr)
  library(tibble)
  library(Matrix)
  library(igraph)
  library(stopwords)
})

set.seed(20260302)

propuestas_path <- file.path(outputs_dir, "propuestas_supervised.parquet")
embeddings_path <- file.path(outputs_dir, "embeddings.parquet")

if (!file.exists(propuestas_path)) stop("Falta outputs/propuestas_supervised.parquet", call. = FALSE)
if (!file.exists(embeddings_path)) stop("Falta outputs/embeddings.parquet", call. = FALSE)

propuestas <- read_parquet(propuestas_path)
embeddings <- read_parquet(embeddings_path)

propuestas <- propuestas |>
  mutate(
    proposal_text_norm = proposal_text |>
      enc2utf8() |>
      stringi::stri_trans_general("Any-Latin; Latin-ASCII") |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9 ]+", " ") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  )

boilerplate_regex <- paste(
  c(
    "plan estrategico de desarrollo nacional al 2050",
    "pedn al 2050",
    "objetivo nacional\\s+on\\s*\\.?\\s*[0-9\\.]+",
    "objetivo estrategico\\s+oe\\s*\\.?\\s*[0-9\\.]+",
    "en concordancia con el oe",
    "del pedn al 2050",
    "plan de gobierno\\s+2026\\s*2031",
    "lineamientos estrategicos por objetivo"
  ),
  collapse = "|"
)

strip_boilerplate <- function(txt) {
  txt |>
    str_replace_all(regex(boilerplate_regex, ignore_case = TRUE), " ") |>
    str_replace_all("\\b(on|oe)\\s*\\.?\\s*[0-9\\.]+\\b", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

propuestas <- propuestas |>
  mutate(
    duplicate_text_norm = strip_boilerplate(proposal_text_norm)
  )

# -------------------------------------------------------------------
# 1) Duplicados y near-duplicates interpartido
# -------------------------------------------------------------------

stop_es <- stopwords("es")
stop_extra <- c(
  "objetivo", "objetivos", "estrategico", "estrategicos", "nacional", "nacionales",
  "plan", "planes", "gobierno", "desarrollo", "calidad", "pais", "peru", "publica",
  "publico", "regional", "regionales", "implementacion", "fortalecer", "garantizar"
)
stop_es <- unique(c(stop_es, stop_extra))

make_token_set <- function(txt) {
  toks <- unlist(str_split(txt, " ", simplify = FALSE), use.names = FALSE)
  toks <- toks[nchar(toks) >= 3]
  toks <- toks[!(toks %in% stop_es)]
  unique(toks)
}

token_sets <- map(propuestas$duplicate_text_norm, make_token_set)
names(token_sets) <- propuestas$proposal_id

expand_exact_pairs <- function(df_group) {
  if (nrow(df_group) < 2) return(tibble())
  idx <- t(combn(seq_len(nrow(df_group)), 2))
  out <- tibble(
    proposal_id_a = df_group$proposal_id[idx[, 1]],
    proposal_id_b = df_group$proposal_id[idx[, 2]],
    party_a = df_group$party[idx[, 1]],
    party_b = df_group$party[idx[, 2]],
    cosine_similarity = 1,
    jaccard_similarity = 1,
    duplicate_type = "exact_duplicate"
  ) |>
    filter(party_a != party_b)
  out
}

exact_groups <- propuestas |>
  filter(nchar(duplicate_text_norm) >= 80) |>
  group_by(duplicate_text_norm) |>
  filter(n_distinct(party) > 1) |>
  group_split()

exact_pairs <- map_dfr(exact_groups, expand_exact_pairs)

# Near duplicates using embedding cosine + lexical jaccard
prop_emb <- propuestas |>
  select(proposal_id, party, proposal_text_norm, duplicate_text_norm) |>
  left_join(embeddings, by = c("proposal_id", "party"))

dim_cols <- names(prop_emb)[str_detect(names(prop_emb), "^dim_")]

X <- as.matrix(prop_emb[, dim_cols, drop = FALSE])
row_ok <- complete.cases(X)

X <- X[row_ok, , drop = FALSE]
prop_emb_ok <- prop_emb[row_ok, ]

row_norm <- sqrt(rowSums(X^2))
row_norm[row_norm == 0] <- 1
Xn <- X / row_norm
sim <- tcrossprod(Xn)
diag(sim) <- 0

cand_idx <- which(sim >= 0.93, arr.ind = TRUE)
cand_idx <- cand_idx[cand_idx[, 1] < cand_idx[, 2], , drop = FALSE]

near_pairs <- tibble()
if (nrow(cand_idx) > 0) {
  near_pairs <- tibble(
    idx_a = cand_idx[, 1],
    idx_b = cand_idx[, 2],
    proposal_id_a = prop_emb_ok$proposal_id[idx_a],
    proposal_id_b = prop_emb_ok$proposal_id[idx_b],
    party_a = prop_emb_ok$party[idx_a],
    party_b = prop_emb_ok$party[idx_b],
    cosine_similarity = sim[cand_idx]
  ) |>
    filter(party_a != party_b) |>
    mutate(
      tokens_a_n = map_int(proposal_id_a, ~ length(token_sets[[.x]])),
      tokens_b_n = map_int(proposal_id_b, ~ length(token_sets[[.x]])),
      jaccard_similarity = map2_dbl(proposal_id_a, proposal_id_b, function(a, b) {
        ta <- token_sets[[a]]
        tb <- token_sets[[b]]
        if (length(ta) == 0 || length(tb) == 0) return(0)
        length(intersect(ta, tb)) / length(union(ta, tb))
      })
    ) |>
    filter(tokens_a_n >= 8, tokens_b_n >= 8) |>
    filter(jaccard_similarity >= 0.65 | (cosine_similarity >= 0.985 & jaccard_similarity >= 0.45)) |>
    mutate(duplicate_type = "near_duplicate") |>
    select(proposal_id_a, proposal_id_b, party_a, party_b, cosine_similarity, jaccard_similarity, duplicate_type)
}

pairs_all <- bind_rows(exact_pairs, near_pairs) |>
  mutate(
    pair_id = if_else(proposal_id_a < proposal_id_b,
      paste(proposal_id_a, proposal_id_b, sep = "__"),
      paste(proposal_id_b, proposal_id_a, sep = "__")
    )
  ) |>
  group_by(pair_id) |>
  arrange(desc(duplicate_type == "exact_duplicate"), desc(cosine_similarity), .by_group = TRUE) |>
  slice(1) |>
  ungroup()

greedy_match_pairs <- function(df_group) {
  if (nrow(df_group) <= 1) return(df_group)
  used_a <- character()
  used_b <- character()
  keep <- logical(nrow(df_group))

  for (i in seq_len(nrow(df_group))) {
    ida <- df_group$proposal_id_a[i]
    idb <- df_group$proposal_id_b[i]
    if (!(ida %in% used_a) && !(idb %in% used_b)) {
      keep[i] <- TRUE
      used_a <- c(used_a, ida)
      used_b <- c(used_b, idb)
    }
  }

  df_group[keep, , drop = FALSE]
}

pairs_all <- pairs_all |>
  mutate(
    party_pair = if_else(
      party_a < party_b,
      paste(party_a, party_b, sep = "__"),
      paste(party_b, party_a, sep = "__")
    ),
    match_score = if_else(duplicate_type == "exact_duplicate", 2, 1) +
      cosine_similarity + jaccard_similarity
  ) |>
  arrange(desc(match_score)) |>
  group_by(party_pair) |>
  group_modify(~ greedy_match_pairs(.x)) |>
  ungroup() |>
  select(-party_pair, -match_score)

# Attach context
pairs_all <- pairs_all |>
  left_join(propuestas |> select(proposal_id, section_guess, axis_supervised, instrument_supervised, source_snippet),
            by = c("proposal_id_a" = "proposal_id")) |>
  rename(section_a = section_guess, axis_a = axis_supervised, instrument_a = instrument_supervised, snippet_a = source_snippet) |>
  left_join(propuestas |> select(proposal_id, section_guess, axis_supervised, instrument_supervised, source_snippet),
            by = c("proposal_id_b" = "proposal_id")) |>
  rename(section_b = section_guess, axis_b = axis_supervised, instrument_b = instrument_supervised, snippet_b = source_snippet)

write_parquet(pairs_all, file.path(outputs_dir, "duplicates_interparty.parquet"))
write_csv(pairs_all, file.path(outputs_dir, "duplicates_interparty.csv"))

# Clusters of duplicates
if (nrow(pairs_all) > 0) {
  g <- graph_from_data_frame(
    pairs_all |>
      transmute(from = proposal_id_a, to = proposal_id_b),
    directed = FALSE
  )

  comp <- components(g)
  cluster_df <- tibble(
    proposal_id = names(comp$membership),
    duplicate_cluster_id = paste0("dup_cluster_", comp$membership),
    cluster_size = comp$csize[comp$membership]
  ) |>
    left_join(propuestas |> select(proposal_id, party, doc_id, axis_supervised, instrument_supervised), by = "proposal_id")

  cluster_df <- cluster_df |>
    group_by(duplicate_cluster_id) |>
    mutate(cluster_party_n = n_distinct(party)) |>
    ungroup() |>
    filter(cluster_party_n > 1)
} else {
  cluster_df <- tibble(
    proposal_id = character(), duplicate_cluster_id = character(), cluster_size = integer(),
    party = character(), doc_id = character(), axis_supervised = character(), instrument_supervised = character(),
    cluster_party_n = integer()
  )
}

write_parquet(cluster_df, file.path(outputs_dir, "duplicate_clusters.parquet"))
write_csv(cluster_df, file.path(outputs_dir, "duplicate_clusters.csv"))

dup_summary_party <- cluster_df |>
  count(party, name = "duplicated_proposals_n") |>
  arrange(desc(duplicated_proposals_n))

write_parquet(dup_summary_party, file.path(outputs_dir, "duplicates_summary_party.parquet"))
write_csv(dup_summary_party, file.path(outputs_dir, "duplicates_summary_party.csv"))

# -------------------------------------------------------------------
# 2) Contradicciones internas por partido
# -------------------------------------------------------------------

rules <- tribble(
  ~dimension, ~stance_a, ~stance_b, ~regex_a, ~regex_b,
  "tax_policy", "raise_tax", "cut_tax",
  "aumentar\\s+(impuesto|tribut)|incrementar\\s+recaud|mayor\\s+carga\\s+tribut",
  "reducir\\s+(impuesto|tribut)|bajar\\s+(impuesto|tribut)|eliminar\\s+(impuesto|tribut)|exoner",

  "fiscal_spending", "expand_spending", "austerity_cut",
  "aumentar\\s+presupuesto|incrementar\\s+gasto|mayor\\s+inversion\\s+public|subsidio",
  "austeridad|reducir\\s+gasto|recorte\\s+presupuest|eliminar\\s+subsid",

  "state_structure", "expand_state", "shrink_state",
  "crear\\s+(ministerio|agencia|oficina|instituto|secretar)",
  "fusion\\s+de\\s+ministerios|reducir\\s+el\\s+estado|eliminar\\s+(ministerio|organismo)",

  "resource_policy", "extractivist", "restrictive",
  "promover\\s+(mineria|hidrocarb|petroleo|gas)|ampliar\\s+explotaci|reactivar\\s+mineria",
  "prohibir\\s+(mineria|hidrocarb|petroleo|gas)|moratoria\\s+miner|cierre\\s+de\\s+minas|sin\\s+hidrocarburos",

  "trade_policy", "open_trade", "protectionist",
  "apertura\\s+comercial|libre\\s+comercio|tratad[o|os]\\s+de\\s+libre\\s+comercio|reducir\\s+arancel",
  "subir\\s+arancel|proteger\\s+industria\\s+nacional|sustituci[oó]n\\s+de\\s+import|restricci[oó]n\\s+import",

  "decentralization", "decentralize", "centralize",
  "descentralizar|transferir\\s+competencias\\s+a\\s+regiones|autonomia\\s+regional",
  "centralizar|mando\\s+unico\\s+nacional|concentrar\\s+competencias"
)

props_lower <- propuestas |>
  mutate(text_lower = str_to_lower(proposal_text))

build_rule_hits <- function(rule_row) {
  pos <- props_lower |>
    filter(str_detect(text_lower, regex(rule_row$regex_a, ignore_case = TRUE))) |>
    mutate(dimension = rule_row$dimension, stance = rule_row$stance_a)

  neg <- props_lower |>
    filter(str_detect(text_lower, regex(rule_row$regex_b, ignore_case = TRUE))) |>
    mutate(dimension = rule_row$dimension, stance = rule_row$stance_b)

  bind_rows(pos, neg)
}

hits <- map_dfr(split(rules, seq_len(nrow(rules))), build_rule_hits) |>
  distinct(proposal_id, dimension, stance, .keep_all = TRUE)

hits_clean <- hits |>
  count(proposal_id, dimension, name = "stance_n") |>
  filter(stance_n == 1) |>
  select(proposal_id, dimension)

hits <- hits |>
  inner_join(hits_clean, by = c("proposal_id", "dimension"))

if (nrow(hits) > 0) {
  hits_top <- hits |>
    mutate(signal = (coalesce(concreteness_score, 0) / 100) * 0.5 + coalesce(axis_supervised_prob, 0) * 0.3 + coalesce(instrument_supervised_prob, 0) * 0.2) |>
    group_by(party, dimension, stance) |>
    arrange(desc(signal), .by_group = TRUE) |>
    slice_head(n = 1) |>
    ungroup()

  contradictions <- rules |>
    select(dimension, stance_a, stance_b) |>
    distinct() |>
    inner_join(
      hits_top,
      by = c("dimension" = "dimension", "stance_a" = "stance")
    ) |>
    rename(party_a = party) |>
    rename_with(~ paste0(.x, "_a"), c("proposal_id", "proposal_text", "section_guess", "source_snippet", "signal", "axis_supervised", "instrument_supervised")) |>
    inner_join(
      hits_top,
      by = c("dimension" = "dimension", "party_a" = "party", "stance_b" = "stance"),
      relationship = "many-to-many"
    ) |>
    rename_with(~ paste0(.x, "_b"), c("proposal_id", "proposal_text", "section_guess", "source_snippet", "signal", "axis_supervised", "instrument_supervised")) |>
    filter(proposal_id_a != proposal_id_b) |>
    mutate(
      contradiction_score = round(100 * ((signal_a + signal_b) / 2), 1),
      contradiction_id = paste0("ctr_", str_pad(row_number(), 5, pad = "0"))
    ) |>
    transmute(
      contradiction_id,
      party = party_a,
      dimension,
      stance_a,
      proposal_id_a,
      section_a = section_guess_a,
      axis_a = axis_supervised_a,
      instrument_a = instrument_supervised_a,
      proposal_text_a,
      snippet_a = source_snippet_a,
      stance_b,
      proposal_id_b,
      section_b = section_guess_b,
      axis_b = axis_supervised_b,
      instrument_b = instrument_supervised_b,
      proposal_text_b,
      snippet_b = source_snippet_b,
      contradiction_score
    ) |>
    mutate(
      pair_id = if_else(proposal_id_a < proposal_id_b,
        paste(proposal_id_a, proposal_id_b, dimension, sep = "__"),
        paste(proposal_id_b, proposal_id_a, dimension, sep = "__")
      )
    ) |>
    distinct(pair_id, .keep_all = TRUE) |>
    select(-pair_id) |>
    arrange(desc(contradiction_score))

} else {
  contradictions <- tibble(
    contradiction_id = character(), party = character(), dimension = character(),
    stance_a = character(), proposal_id_a = character(), section_a = character(), axis_a = character(),
    instrument_a = character(), proposal_text_a = character(), snippet_a = character(),
    stance_b = character(), proposal_id_b = character(), section_b = character(), axis_b = character(),
    instrument_b = character(), proposal_text_b = character(), snippet_b = character(),
    contradiction_score = numeric()
  )
}

write_parquet(contradictions, file.path(outputs_dir, "contradictions_party.parquet"))
write_csv(contradictions, file.path(outputs_dir, "contradictions_party.csv"))

contr_summary <- contradictions |>
  group_by(party, dimension) |>
  summarise(contradictions_n = n(), avg_score = mean(contradiction_score, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(contradictions_n), desc(avg_score))

write_parquet(contr_summary, file.path(outputs_dir, "contradictions_summary_party.parquet"))
write_csv(contr_summary, file.path(outputs_dir, "contradictions_summary_party.csv"))

# -------------------------------------------------------------------
# 3) Viabilidad fiscal
# -------------------------------------------------------------------

parse_num_es <- function(x) {
  x <- str_trim(x)
  if (is.na(x) || x == "") return(NA_real_)

  if (str_detect(x, "\\.") && str_detect(x, ",")) {
    x <- str_replace_all(x, "\\.", "")
    x <- str_replace_all(x, ",", ".")
  } else if (str_detect(x, ",")) {
    x <- str_replace_all(x, ",", ".")
  } else if (str_count(x, "\\.") > 1) {
    x <- str_replace_all(x, "\\.", "")
  }

  suppressWarnings(as.numeric(x))
}

extract_amount_info <- function(text) {
  tx <- str_to_lower(text)
  has_context <- str_detect(tx, "s/\\.?|soles|presupuesto|inversi[oó]n|gasto|financ|millones|mil millones")

  m <- str_match_all(tx, "(?:s/\\.?\\s*)?([0-9]{1,3}(?:[\\.,][0-9]{3})*(?:[\\.,][0-9]+)?|[0-9]+(?:[\\.,][0-9]+)?)\\s*(mil\\s+millones|millones|miles)?")[[1]]

  if (nrow(m) == 0 || !has_context) {
    return(list(monetary_mentions = 0L, estimated_amount_pen = NA_real_, amount_tokens = NA_character_))
  }

  nums <- map_dbl(m[, 2], parse_num_es)
  units <- m[, 3]
  mult <- case_when(
    str_detect(units, "mil\\s+millones") ~ 1e9,
    str_detect(units, "millones") ~ 1e6,
    str_detect(units, "miles") ~ 1e3,
    TRUE ~ 1
  )

  vals <- nums * mult
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) == 0) {
    return(list(monetary_mentions = 0L, estimated_amount_pen = NA_real_, amount_tokens = NA_character_))
  }

  list(
    monetary_mentions = as.integer(length(vals)),
    estimated_amount_pen = max(vals, na.rm = TRUE),
    amount_tokens = paste(head(round(vals, 2), 5), collapse = ";")
  )
}

macro_anchor_regex <- "regla fiscal|d[eé]ficit fiscal|deuda p[uú]blica|presi[oó]n tributaria|equilibrio fiscal|sostenibilidad fiscal|marco macro|mef"

fiscal_df <- propuestas |>
  mutate(
    amount_info = map(proposal_text, extract_amount_info),
    monetary_mentions = map_int(amount_info, "monetary_mentions"),
    estimated_amount_pen = map_dbl(amount_info, "estimated_amount_pen"),
    amount_tokens = map_chr(amount_info, "amount_tokens"),
    has_macro_anchor = str_detect(str_to_lower(proposal_text), macro_anchor_regex),
    fiscal_impact_band = case_when(
      !is.na(estimated_amount_pen) & estimated_amount_pen >= 1e10 ~ "high",
      !is.na(estimated_amount_pen) & estimated_amount_pen >= 1e8 ~ "medium",
      mentions_cost | monetary_mentions > 0 ~ "low",
      TRUE ~ "unknown"
    ),
    high_impact_without_funding = fiscal_impact_band == "high" & !mentions_funding_source,
    fiscal_viability_score = 50 +
      if_else(mentions_funding_source, 15, 0) +
      if_else(mentions_cost | monetary_mentions > 0, 10, 0) +
      if_else(has_time_horizon, 10, 0) +
      if_else(has_quant_target, 10, 0) +
      if_else(instrument_supervised != "unspecified", 10, 0) +
      if_else(has_macro_anchor, 10, 0) +
      if_else(!is.na(evidence_citation_guess), 5, 0) -
      if_else(high_impact_without_funding, 20, 0) -
      if_else((mentions_cost | monetary_mentions > 0) & !mentions_funding_source, 10, 0),
    fiscal_viability_score = pmax(0, pmin(100, fiscal_viability_score)),
    fiscal_viability_tier = case_when(
      fiscal_viability_score >= 70 ~ "alta",
      fiscal_viability_score >= 45 ~ "media",
      TRUE ~ "baja"
    ),
    fiscal_risk_flag = case_when(
      high_impact_without_funding ~ "alto_riesgo",
      fiscal_viability_score < 45 ~ "riesgo",
      TRUE ~ "sin_alerta"
    )
  ) |>
  select(
    party, doc_id, proposal_id, axis_supervised, instrument_supervised,
    proposal_text, source_snippet,
    mentions_cost, mentions_funding_source, has_quant_target, has_time_horizon,
    monetary_mentions, amount_tokens, estimated_amount_pen, fiscal_impact_band,
    has_macro_anchor, high_impact_without_funding,
    fiscal_viability_score, fiscal_viability_tier, fiscal_risk_flag
  )

write_parquet(fiscal_df, file.path(outputs_dir, "fiscal_viability.parquet"))
write_csv(fiscal_df, file.path(outputs_dir, "fiscal_viability.csv"))

fiscal_party <- fiscal_df |>
  group_by(party) |>
  summarise(
    proposals_n = n(),
    avg_fiscal_viability = mean(fiscal_viability_score, na.rm = TRUE),
    pct_alta_viabilidad = mean(fiscal_viability_tier == "alta", na.rm = TRUE),
    pct_baja_viabilidad = mean(fiscal_viability_tier == "baja", na.rm = TRUE),
    high_impact_without_funding_n = sum(high_impact_without_funding, na.rm = TRUE),
    mentions_funding_pct = mean(mentions_funding_source, na.rm = TRUE),
    macro_anchor_pct = mean(has_macro_anchor, na.rm = TRUE),
    est_total_amount_pen = sum(estimated_amount_pen, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_fiscal_viability))

write_parquet(fiscal_party, file.path(outputs_dir, "fiscal_viability_party.parquet"))
write_csv(fiscal_party, file.path(outputs_dir, "fiscal_viability_party.csv"))

# Minimal marco fiscal reference (textual)
marco_ref <- tibble(
  marco_element = c(
    "Disciplina fiscal",
    "Sostenibilidad de deuda",
    "Regla de deficit",
    "Priorizacion de gasto",
    "Financiamiento identificable"
  ),
  reference_note = c(
    "Consistencia entre compromisos y espacio fiscal.",
    "Evitar senda de deuda explosiva por promesas no financiadas.",
    "Evitar ampliaciones de gasto sin trayectoria de consolidacion.",
    "Focalizar gasto en medidas con mayor retorno publico.",
    "Toda propuesta costosa deberia declarar fuente de financiamiento."
  )
)

write_csv(marco_ref, file.path(outputs_dir, "fiscal_marco_reference.csv"))

# Export summary
diag_summary <- tibble(
  metric = c(
    "duplicate_pairs_interparty",
    "duplicate_clusters_interparty",
    "contradictions_detected",
    "parties_with_contradictions",
    "avg_fiscal_viability_all",
    "high_impact_without_funding_all"
  ),
  value = c(
    nrow(pairs_all),
    n_distinct(cluster_df$duplicate_cluster_id),
    nrow(contradictions),
    n_distinct(contradictions$party),
    round(mean(fiscal_df$fiscal_viability_score, na.rm = TRUE), 2),
    sum(fiscal_df$high_impact_without_funding, na.rm = TRUE)
  )
)

write_csv(diag_summary, file.path(outputs_dir, "diagnostics_block_summary.csv"))

message("Bloque avanzado completado:")
message(sprintf(" - Duplicados interpartido: %d pares", nrow(pairs_all)))
message(sprintf(" - Contradicciones internas: %d", nrow(contradictions)))
message(sprintf(" - Viabilidad fiscal promedio: %.2f", mean(fiscal_df$fiscal_viability_score, na.rm = TRUE)))
