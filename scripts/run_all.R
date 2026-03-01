#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source_txt_dir <- "/Users/noam/Library/CloudStorage/GoogleDrive-lopeznoam@gmail.com/Mi unidad/Proyectos con IA/Planes_gobierno/Planes"
data_dir <- file.path(project_root, "data", "plans_txt")
outputs_dir <- file.path(project_root, "outputs")
site_dir <- file.path(project_root, "site")

if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)
if (!dir.exists(file.path(project_root, "scripts"))) dir.create(file.path(project_root, "scripts"), recursive = TRUE)
if (!dir.exists(site_dir)) dir.create(site_dir, recursive = TRUE)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

core_pkgs <- c(
  "fs", "stringr", "stringi", "dplyr", "tidyr", "purrr", "readr", "tibble",
  "arrow", "tokenizers", "stopwords", "tidytext", "Matrix", "irlba", "uwot",
  "stm", "tm", "plotly", "DT", "visNetwork", "igraph", "dbarts", "glue", "jsonlite", "renv", "knitr", "rmarkdown", "pdftools"
)
install_if_missing(core_pkgs)

suppressPackageStartupMessages({
  library(fs)
  library(stringr)
  library(stringi)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(arrow)
  library(tokenizers)
  library(stopwords)
  library(tidytext)
  library(Matrix)
  library(irlba)
  library(uwot)
  library(stm)
  library(glue)
  library(renv)
})

set.seed(20260228)

# renv setup
try({
  renv::activate(project = project_root)
}, silent = TRUE)

# Step 1: copy source .txt files if present
source_txt <- character(0)
if (dir.exists(source_txt_dir)) {
  source_txt <- dir_ls(source_txt_dir, recurse = TRUE, type = "file", regexp = "(?i)\\.txt$")
}

if (length(source_txt) > 0) {
  target_paths <- path(data_dir, path_file(source_txt))
  file_copy(source_txt, target_paths, overwrite = TRUE)
}

# Step 2: read local copied plans
all_txt_files <- dir_ls(data_dir, recurse = TRUE, type = "file", regexp = "(?i)\\.txt$")
if (length(all_txt_files) == 0) {
  stop("No se encontraron archivos .txt en data/plans_txt/.", call. = FALSE)
}

plan_files <- all_txt_files[!str_detect(path_file(all_txt_files), regex("resumen\\.txt$", ignore_case = TRUE))]
if (length(plan_files) == 0) {
  stop("No se encontraron planes completos (sin 'resumen') en data/plans_txt/.", call. = FALSE)
}

message(glue("Archivos txt detectados: {length(all_txt_files)}"))
message(glue("Planes completos usados: {length(plan_files)}"))

normalize_utf8 <- function(x) {
  x <- enc2utf8(x)
  x <- str_replace_all(x, "\\r\\n?", "\\n")
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_replace_all(x, "[\\t ]+", " ")
  x <- str_replace_all(x, "[ ]*\\n[ ]*", "\\n")
  x <- str_replace_all(x, "\\n{3,}", "\\n\\n")
  str_trim(x)
}

file_to_party <- function(file_path) {
  base <- path_ext_remove(path_file(file_path))
  parts <- str_split(base, " - ", n = 2, simplify = TRUE)
  party <- str_trim(parts[, 1])
  party
}

file_to_doc_id <- function(file_path) {
  base <- path_ext_remove(path_file(file_path))
  base_ascii <- stri_trans_general(base, "Any-Latin; Latin-ASCII")
  base_ascii <- str_to_lower(base_ascii)
  base_ascii <- str_replace_all(base_ascii, "[^a-z0-9]+", "_")
  base_ascii <- str_replace_all(base_ascii, "^_+|_+$", "")
  paste0("doc_", base_ascii)
}

is_heading_line <- function(line) {
  ln <- str_squish(line)
  if (ln == "" || nchar(ln) < 4 || nchar(ln) > 140) return(FALSE)

  numbered <- str_detect(ln, "^(\\d+(\\.\\d+)*|[IVXLCM]+)[\\.)-]?\\s+[A-ZÁÉÍÓÚÑ].+")
  upperish <- ln == str_to_upper(ln) && str_detect(ln, "[A-ZÁÉÍÓÚÑ]")
  titleish <- str_detect(ln, "^[A-ZÁÉÍÓÚÑ][A-Za-zÁÉÍÓÚáéíóúÑñÜü ]{3,80}$") &&
    str_count(ln, "\\s") <= 8

  numbered || upperish || titleish
}

split_sections <- function(text) {
  lines <- str_split(text, "\\n", simplify = FALSE)[[1]]
  lines <- lines[str_trim(lines) != ""]

  section_id <- 1L
  current_heading <- "INTRODUCCION"
  out <- vector("list", length(lines))

  for (i in seq_along(lines)) {
    ln <- str_squish(lines[[i]])
    if (is_heading_line(ln)) {
      current_heading <- str_sub(ln, 1, 100)
      section_id <- section_id + 1L
      out[[i]] <- tibble(section_id = section_id, section_guess = current_heading, line = "")
    } else {
      out[[i]] <- tibble(section_id = section_id, section_guess = current_heading, line = ln)
    }
  }

  bind_rows(out) |>
    group_by(section_id, section_guess) |>
    summarise(section_text = str_trim(paste(line, collapse = "\\n")), .groups = "drop") |>
    filter(section_text != "")
}

proposal_verbs <- paste(
  c(
    "crear", "creara", "crearan", "implementar", "implementara", "fortalecer", "fortalecera",
    "reformar", "reformara", "aumentar", "incrementar", "reducir", "eliminar", "construir",
    "promover", "impulsar", "garantizar", "establecer", "desarrollar", "financiar", "priorizar",
    "ampliar", "modernizar", "descentralizar", "reorganizar", "digitalizar", "formalizar"
  ),
  collapse = "|"
)

extract_proposals_from_section <- function(section_text) {
  lines <- str_split(section_text, "\\n", simplify = FALSE)[[1]]

  bullets <- lines[str_detect(lines, "^\\s*(?:[-•*]|\\d+[\\.)]|[a-zA-Z]\\))\\s+")]
  bullets <- str_replace(bullets, "^\\s*(?:[-•*]|\\d+[\\.)]|[a-zA-Z]\\))\\s+", "")

  sents <- tokenizers::tokenize_sentences(section_text, lowercase = FALSE, strip_punct = FALSE)[[1]]
  sents <- str_squish(sents)
  sents <- sents[nchar(sents) >= 35]
  action_sents <- sents[str_detect(str_to_lower(sents), glue("\\b({proposal_verbs})\\w*\\b"))]

  cands <- unique(c(str_squish(bullets), action_sents))
  cands <- cands[nchar(cands) >= 35 & nchar(cands) <= 800]
  cands
}

axis_dict <- list(
  seguridad = c("seguridad", "pnp", "policia", "delito", "violencia", "extorsion", "carcel", "inseguridad"),
  economia = c("economia", "pbi", "inflacion", "tribut", "impuesto", "inversion", "export", "empresa", "mype", "productividad"),
  salud = c("salud", "hospital", "medic", "essalud", "vacuna", "atencion", "sanitario"),
  educacion = c("educacion", "colegio", "universidad", "docente", "curriculo", "beca", "aprendizaje"),
  energia = c("energia", "gas", "petroleo", "electric", "hidrocarb", "renovable", "solar", "eolica"),
  empleo = c("empleo", "trabajo", "laboral", "salario", "formalizacion", "desempleo"),
  institucionalidad = c("estado", "reforma", "constitucion", "congreso", "justicia", "corrupcion", "institucional", "gobernanza"),
  ambiente = c("ambient", "clima", "agua", "forest", "residuo", "contamin", "sostenible"),
  infraestructura = c("infraestructura", "carretera", "puente", "transporte", "tren", "obra", "vivienda", "saneamiento"),
  social = c("pobreza", "inclusion", "mujer", "joven", "ninez", "adulto mayor", "discapacidad", "proteccion social")
)

detect_axis <- function(text) {
  tx <- str_to_lower(text)
  counts <- map_dbl(axis_dict, function(keys) {
    sum(map_dbl(keys, ~ str_count(tx, fixed(.x))))
  })
  if (sum(counts) == 0) {
    return(list(axis = "otros", confidence = 0))
  }
  best <- names(which.max(counts))
  conf <- as.numeric(max(counts) / sum(counts))
  list(axis = best, confidence = conf)
}

detect_instrument <- function(text) {
  tx <- str_to_lower(text)
  if (str_detect(tx, "ley|reforma|constituci[oó]n|decreto|reglament")) return("law/reform")
  if (str_detect(tx, "programa|plan |estrategia|beca|subsidio|bono")) return("program")
  if (str_detect(tx, "inversi[oó]n|presupuesto|gasto|financ|construcci[oó]n|obra")) return("spending/investment")
  if (str_detect(tx, "ministerio|secretar[ií]a|instituto|agencia|oficina|autoridad|sistema nacional")) return("institutional change")
  if (str_detect(tx, "sanci[oó]n|pena|fiscaliz|control|polic[ií]a|c[aá]rcel|multa|delito")) return("enforcement/punitive")
  if (str_detect(tx, "digital|tecnolog|datos|inteligencia artificial|plataforma|interoperab|ciber")) return("technology/data")
  "unspecified"
}

detect_population_target <- function(text) {
  tx <- str_to_lower(text)
  keys <- c("joven", "mujer", "ni[ñn]", "adulto mayor", "poblaci[oó]n vulnerable", "mype", "empresa", "polic[ií]a", "docente", "estudiante", "agricultor", "trabajador", "personas con discapacidad")
  hits <- keys[str_detect(tx, keys)]
  if (length(hits) == 0) NA_character_ else paste(unique(hits), collapse = "; ")
}

detect_territory_target <- function(text) {
  tx <- str_to_lower(text)
  keys <- c(
    "per[uú]", "nacional", "lima", "regiones", "provincia", "distrito", "rural", "urbana", "amazonas", "costa", "sierra", "selva",
    "arequipa", "cusco", "piura", "la libertad", "loreto", "puno", "junin", "san martin", "huancavelica", "ayacucho"
  )
  hits <- keys[str_detect(tx, keys)]
  if (length(hits) == 0) NA_character_ else paste(unique(hits), collapse = "; ")
}

detect_evidence <- function(text) {
  tx <- str_to_lower(text)
  m <- str_match(tx, "(inei|bcrp|mef|ocde|cepal|oms|pisa|seg[uú]n [^,.;]{3,40}|evidencia|estudio[s]?|diagn[oó]stico)")
  out <- m[, 2]
  ifelse(is.na(out) | out == "", NA_character_, out)
}

extract_numbers <- function(text) {
  nums <- str_extract_all(text, "\\b\\d+[\\d\\.,]*%?\\b")[[1]]
  nums <- unique(nums)
  if (length(nums) == 0) NA_character_ else paste(nums, collapse = ";")
}

detect_quant_target <- function(text) {
  tx <- str_to_lower(text)
  has_num <- str_detect(tx, "\\b\\d+[\\d\\.,]*\\b")
  with_target <- str_detect(tx, "%|por ciento|meta|al\\s+\\d+|hasta\\s+\\d+|millones|miles|pbi|ratio")
  has_num && with_target
}

detect_time_horizon <- function(text) {
  tx <- str_to_lower(text)
  str_detect(tx, "\\b20\\d{2}(?:\\s*[-–]\\s*20\\d{2})?\\b|\\b\\d+\\s*(d[ií]as|meses|a[ñn]os)\\b|corto plazo|mediano plazo|largo plazo|en 100 d[ií]as")
}

detect_cost <- function(text) {
  tx <- str_to_lower(text)
  str_detect(tx, "s/\\.?|soles|millones|miles de millones|presupuesto|costo|gasto")
}

detect_funding <- function(text) {
  tx <- str_to_lower(text)
  str_detect(tx, "financ|canon|minero|impuesto|recaudaci[oó]n|app|asociaci[oó]n p[uú]blico privada|cooperaci[oó]n|deuda|bono soberano")
}

count_tokens <- function(text) {
  str_count(text, "\\b[[:alpha:]áéíóúñüÁÉÍÓÚÑÜ]+\\b")
}

snippet_around <- function(doc_text, proposal_text, n = 250) {
  doc_l <- str_to_lower(doc_text)
  needle <- str_to_lower(str_squish(proposal_text))
  needle <- str_sub(needle, 1, min(120, nchar(needle)))
  loc <- str_locate(doc_l, fixed(needle))

  if (is.na(loc[1])) {
    return(str_sub(str_squish(proposal_text), 1, 2 * n))
  }
  s <- max(1, loc[1] - n)
  e <- min(nchar(doc_text), loc[2] + n)
  str_sub(doc_text, s, e)
}

is_vague <- function(text, instrument_type, has_quant_target, has_time_horizon) {
  tx <- str_to_lower(text)
  vague_verbs <- str_detect(tx, "fortalecer|promover|impulsar|mejorar|potenciar")
  weak_specificity <- !has_quant_target && !has_time_horizon && instrument_type == "unspecified"
  vague_verbs && weak_specificity
}

score_concreteness <- function(has_quant_target, has_time_horizon, instrument_type,
                               population_target, territory_target, mentions_cost,
                               mentions_funding_source, evidence_citation_guess, vague_flag) {
  score <- 0
  if (isTRUE(has_quant_target)) score <- score + 20
  if (isTRUE(has_time_horizon)) score <- score + 15
  if (!is.na(instrument_type) && instrument_type != "unspecified") score <- score + 15
  if (!is.na(population_target) && population_target != "") score <- score + 10
  if (!is.na(territory_target) && territory_target != "") score <- score + 10
  if (isTRUE(mentions_cost) || isTRUE(mentions_funding_source)) score <- score + 10
  if (!is.na(evidence_citation_guess) && evidence_citation_guess != "") score <- score + 10
  if (isTRUE(vague_flag)) score <- score - 10
  score <- max(0, min(100, score))
  score
}

build_proposals <- function(file_path) {
  raw_text <- read_file(file_path)
  text <- normalize_utf8(raw_text)
  party <- file_to_party(file_path)
  doc_id <- file_to_doc_id(file_path)

  sections <- split_sections(text)
  if (nrow(sections) == 0) return(tibble())

  rows <- vector("list", nrow(sections))

  for (i in seq_len(nrow(sections))) {
    sec <- sections[i, ]
    cands <- extract_proposals_from_section(sec$section_text)
    if (length(cands) == 0) next

    rows[[i]] <- tibble(
      party = party,
      doc_id = doc_id,
      section_guess = sec$section_guess,
      proposal_text = cands,
      source_snippet = map_chr(cands, ~ snippet_around(text, .x, n = 250))
    )
  }

  out <- bind_rows(rows)
  if (nrow(out) == 0) return(tibble())

  out <- out |>
    mutate(
      axis_info = map(proposal_text, detect_axis),
      axis = map_chr(axis_info, "axis"),
      axis_confidence = map_dbl(axis_info, "confidence"),
      instrument_type = map_chr(proposal_text, detect_instrument),
      has_quant_target = map_lgl(proposal_text, detect_quant_target),
      has_time_horizon = map_lgl(proposal_text, detect_time_horizon),
      population_target = map_chr(proposal_text, detect_population_target),
      territory_target = map_chr(proposal_text, detect_territory_target),
      mentions_cost = map_lgl(proposal_text, detect_cost),
      mentions_funding_source = map_lgl(proposal_text, detect_funding),
      evidence_citation_guess = map_chr(proposal_text, detect_evidence),
      numbers_found = map_chr(proposal_text, extract_numbers),
      tokens_n = map_int(proposal_text, count_tokens)
    ) |>
    mutate(
      proposal_id = paste0(doc_id, "_p", str_pad(row_number(), width = 4, side = "left", pad = "0")),
      vague_flag = pmap_lgl(
        list(proposal_text, instrument_type, has_quant_target, has_time_horizon),
        is_vague
      ),
      concreteness_score = pmap_dbl(
        list(
          has_quant_target, has_time_horizon, instrument_type,
          population_target, territory_target, mentions_cost,
          mentions_funding_source, evidence_citation_guess, vague_flag
        ),
        score_concreteness
      )
    ) |>
    select(
      party, doc_id, section_guess, axis, axis_confidence, proposal_id, proposal_text,
      instrument_type, has_quant_target, has_time_horizon, population_target,
      territory_target, mentions_cost, mentions_funding_source,
      evidence_citation_guess, numbers_found, tokens_n, source_snippet,
      vague_flag, concreteness_score
    )

  out
}

propuestas <- map_dfr(plan_files, build_proposals)

# Dedup and quality filters
propuestas <- propuestas |>
  mutate(proposal_text = str_squish(proposal_text)) |>
  filter(tokens_n >= 6) |>
  distinct(doc_id, proposal_text, .keep_all = TRUE)

if (nrow(propuestas) == 0) {
  stop("No se extrajeron propuestas. Revisa la segmentacion.", call. = FALSE)
}

# Traceability checks
propuestas <- propuestas |>
  mutate(
    doc_id = if_else(is.na(doc_id) | doc_id == "", "doc_missing", doc_id),
    source_snippet = if_else(is.na(source_snippet) | source_snippet == "", str_sub(proposal_text, 1, 250), source_snippet)
  )

write_csv(propuestas, file.path(outputs_dir, "propuestas.csv"))
write_parquet(propuestas, file.path(outputs_dir, "propuestas.parquet"))

# Step 3: party-axis derived scores
party_axis_scores <- propuestas |>
  group_by(party, axis) |>
  summarise(
    proposals_n = n(),
    axis_share = n() / nrow(filter(propuestas, party == first(party))),
    avg_concreteness = mean(concreteness_score, na.rm = TRUE),
    pct_quant = mean(has_quant_target, na.rm = TRUE),
    pct_time = mean(has_time_horizon, na.rm = TRUE),
    pct_cost = mean(mentions_cost, na.rm = TRUE),
    pct_funding = mean(mentions_funding_source, na.rm = TRUE),
    pct_evidence = mean(!is.na(evidence_citation_guess), na.rm = TRUE),
    .groups = "drop"
  )

party_overall <- propuestas |>
  group_by(party) |>
  summarise(
    axis = "overall",
    proposals_n = n(),
    axis_share = 1,
    avg_concreteness = mean(concreteness_score, na.rm = TRUE),
    pct_quant = mean(has_quant_target, na.rm = TRUE),
    pct_time = mean(has_time_horizon, na.rm = TRUE),
    pct_cost = mean(mentions_cost, na.rm = TRUE),
    pct_funding = mean(mentions_funding_source, na.rm = TRUE),
    pct_evidence = mean(!is.na(evidence_citation_guess), na.rm = TRUE),
    .groups = "drop"
  )

party_axis_scores <- bind_rows(party_axis_scores, party_overall)
write_parquet(party_axis_scores, file.path(outputs_dir, "party_axis_scores.parquet"))

# Step 4: topic modeling with STM
stm_topics_out <- NULL
stm_ok <- TRUE

tryCatch({
  stm_meta <- propuestas |>
    transmute(party = as.factor(party), proposal_id, text = proposal_text)

  tp <- textProcessor(
    documents = stm_meta$text,
    metadata = stm_meta,
    language = "es",
    lowercase = TRUE,
    removestopwords = TRUE,
    removenumbers = TRUE,
    removepunctuation = TRUE,
    stem = FALSE,
    wordLengths = c(3, Inf)
  )

  prep <- prepDocuments(tp$documents, tp$vocab, tp$meta, lower.thresh = 5)
  docs <- prep$documents
  vocab <- prep$vocab
  meta <- prep$meta

  if (length(docs) < 30 || length(vocab) < 100) {
    stop("Corpus insuficiente para STM estable.")
  }

  K <- max(6, min(12, round(sqrt(length(docs)))))

  fit <- stm(
    documents = docs,
    vocab = vocab,
    K = K,
    prevalence = ~ party,
    data = meta,
    init.type = "Spectral",
    max.em.its = 75,
    seed = 20260228,
    verbose = FALSE
  )

  theta <- as_tibble(fit$theta)
  names(theta) <- paste0("topic_", seq_len(ncol(theta)))

  topic_words <- labelTopics(fit, n = 6)$prob
  topic_labels <- map_chr(seq_len(nrow(topic_words)), function(i) {
    paste(topic_words[i, 1:4], collapse = ", ")
  })

  theta_long <- bind_cols(meta |> select(proposal_id, party), theta) |>
    pivot_longer(
      cols = starts_with("topic_"),
      names_to = "topic",
      values_to = "gamma"
    ) |>
    mutate(
      topic_id = as.integer(str_remove(topic, "topic_")),
      topic_label = topic_labels[topic_id]
    )

  dom <- theta_long |>
    group_by(proposal_id) |>
    slice_max(order_by = gamma, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(proposal_id, dominant_topic = topic_id, dominant_gamma = gamma)

  stm_topics_out <- theta_long |>
    left_join(dom, by = "proposal_id") |>
    arrange(proposal_id, desc(gamma))
}, error = function(e) {
  stm_ok <<- FALSE
  message("STM fallback: ", conditionMessage(e))
})

if (!stm_ok || is.null(stm_topics_out)) {
  # fallback topic approximation by kmeans on tf-idf vectors
  toks <- propuestas |>
    select(proposal_id, party, proposal_text) |>
    unnest_tokens(term, proposal_text, token = "words") |>
    filter(str_detect(term, "^[[:alpha:]áéíóúñü]{3,}$")) |>
    filter(!term %in% stopwords("es"))

  term_counts <- toks |>
    count(proposal_id, party, term, sort = FALSE)

  tfidf <- term_counts |>
    bind_tf_idf(term = term, document = proposal_id, n = n)

  mat <- cast_sparse(tfidf, proposal_id, term, tf_idf)
  n_comp <- max(2, min(20, ncol(mat) - 1, nrow(mat) - 1))
  sv <- irlba(mat, nv = n_comp)
  emb <- sv$u %*% diag(sv$d)

  k <- max(4, min(10, floor(sqrt(nrow(emb)))))
  km <- kmeans(emb, centers = k, nstart = 20)

  nearest_terms <- function(center_vec, v, top_n = 4) {
    ord <- order(abs(v %*% center_vec), decreasing = TRUE)
    colnames(mat)[ord[seq_len(min(top_n, length(ord)))]]
  }

  labels <- map_chr(seq_len(k), function(i) {
    ctr <- km$centers[i, ]
    if (length(ctr) != ncol(v <- sv$v)) return("topic")
    terms <- nearest_terms(ctr, sv$v)
    paste(terms, collapse = ", ")
  })

  assign_df <- tibble(
    proposal_id = rownames(mat),
    topic_id = km$cluster
  ) |>
    left_join(propuestas |> select(proposal_id, party), by = "proposal_id") |>
    mutate(
      gamma = 1,
      topic = paste0("topic_", topic_id),
      topic_label = labels[topic_id],
      dominant_topic = topic_id,
      dominant_gamma = 1
    )

  stm_topics_out <- assign_df |>
    select(proposal_id, party, topic, gamma, topic_id, topic_label, dominant_topic, dominant_gamma)
}

write_parquet(stm_topics_out, file.path(outputs_dir, "topics_stm.parquet"))

# Step 5: embeddings / tf-idf fallback
openai_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
use_openai <- nzchar(openai_key)

build_tfidf_embeddings <- function(df) {
  toks <- df |>
    select(proposal_id, party, proposal_text) |>
    unnest_tokens(term, proposal_text, token = "words") |>
    filter(str_detect(term, "^[[:alpha:]áéíóúñü]{3,}$")) |>
    filter(!term %in% stopwords("es"))

  term_counts <- toks |>
    count(proposal_id, party, term, sort = FALSE)

  tfidf <- term_counts |>
    bind_tf_idf(term = term, document = proposal_id, n = n)

  mat <- cast_sparse(tfidf, proposal_id, term, tf_idf)
  n_comp <- max(2, min(50, ncol(mat) - 1, nrow(mat) - 1))

  sv <- irlba(mat, nv = n_comp)
  emb <- sv$u %*% diag(sv$d)

  out <- as_tibble(emb)
  names(out) <- paste0("dim_", seq_len(ncol(out)))
  out <- bind_cols(tibble(proposal_id = rownames(mat)), out) |>
    left_join(df |> select(proposal_id, party), by = "proposal_id") |>
    mutate(embedding_method = "tfidf_svd") |>
    relocate(party, .after = proposal_id)

  list(embedding_df = out, embed_matrix = emb, ids = rownames(mat), method = "tfidf_svd")
}

embedding_bundle <- NULL

if (use_openai) {
  # optional path; fallback to tf-idf if any issue
  tryCatch({
    stop("OPENAI embedding integration not configured in this run; using tf-idf fallback.")
  }, error = function(e) {
    message("Embeddings fallback: ", conditionMessage(e))
    embedding_bundle <<- build_tfidf_embeddings(propuestas)
  })
} else {
  embedding_bundle <- build_tfidf_embeddings(propuestas)
}

embeddings_df <- embedding_bundle$embedding_df
write_parquet(embeddings_df, file.path(outputs_dir, "embeddings.parquet"))

# Step 6: UMAP coordinates
embedding_cols <- names(embeddings_df)[str_detect(names(embeddings_df), "^dim_")]
embed_mat <- as.matrix(embeddings_df[, embedding_cols, drop = FALSE])

umap_coords <- NULL
tryCatch({
  if (nrow(embed_mat) < 4 || ncol(embed_mat) < 2) stop("No hay dimensionalidad suficiente para UMAP.")
  set.seed(20260228)
  um <- uwot::umap(
    embed_mat,
    n_neighbors = min(20, nrow(embed_mat) - 1),
    min_dist = 0.2,
    metric = "cosine",
    n_components = 2,
    verbose = FALSE
  )
  umap_coords <- tibble(
    proposal_id = embeddings_df$proposal_id,
    party = embeddings_df$party,
    umap1 = um[, 1],
    umap2 = um[, 2],
    method = "umap"
  )
}, error = function(e) {
  message("UMAP fallback: ", conditionMessage(e))
  fallback <- embed_mat[, seq_len(min(2, ncol(embed_mat))), drop = FALSE]
  if (ncol(fallback) == 1) fallback <- cbind(fallback, rep(0, nrow(fallback)))
  umap_coords <<- tibble(
    proposal_id = embeddings_df$proposal_id,
    party = embeddings_df$party,
    umap1 = fallback[, 1],
    umap2 = fallback[, 2],
    method = "svd_first2"
  )
})

write_parquet(umap_coords, file.path(outputs_dir, "umap_coords.parquet"))

# Step 7: party similarity
cosine_sim <- function(a, b) {
  num <- sum(a * b)
  den <- sqrt(sum(a * a)) * sqrt(sum(b * b))
  if (den == 0) return(0)
  num / den
}

party_centroids <- embeddings_df |>
  group_by(party) |>
  summarise(across(all_of(embedding_cols), mean), .groups = "drop")

pairs <- expand_grid(party_i = party_centroids$party, party_j = party_centroids$party)

sim_df <- pairs |>
  rowwise() |>
  mutate(
    similarity = {
      ai <- as.numeric(party_centroids[party_centroids$party == party_i, embedding_cols, drop = TRUE])
      bj <- as.numeric(party_centroids[party_centroids$party == party_j, embedding_cols, drop = TRUE])
      cosine_sim(ai, bj)
    }
  ) |>
  ungroup()

write_parquet(sim_df, file.path(outputs_dir, "similarity_party.parquet"))

# Step 8: verification checklist summary
verification <- tibble(
  metric = c(
    "txt_detected_total",
    "plans_full_used",
    "parties_detected",
    "proposals_total",
    "pct_quant_targets",
    "pct_time_horizon",
    "rows_with_doc_id",
    "rows_with_source_snippet"
  ),
  value = c(
    length(all_txt_files),
    length(plan_files),
    n_distinct(propuestas$party),
    nrow(propuestas),
    round(mean(propuestas$has_quant_target) * 100, 2),
    round(mean(propuestas$has_time_horizon) * 100, 2),
    sum(!is.na(propuestas$doc_id) & propuestas$doc_id != ""),
    sum(!is.na(propuestas$source_snippet) & propuestas$source_snippet != "")
  )
)
write_csv(verification, file.path(outputs_dir, "verification_summary.csv"))

party_names <- propuestas |>
  distinct(party) |>
  arrange(party)
write_lines(party_names$party, file.path(outputs_dir, "party_names.txt"))

proposals_per_party <- propuestas |>
  count(party, name = "proposals_n") |>
  arrange(desc(proposals_n))
write_csv(proposals_per_party, file.path(outputs_dir, "proposals_per_party.csv"))

# Step 9: validation + supervised classifiers
val_script <- file.path(project_root, "scripts", "run_validation_and_supervised.R")
if (file.exists(val_script)) {
  val_cmd <- sprintf("Rscript %s", shQuote(val_script))
  val_status <- system(val_cmd)
  if (val_status != 0) {
    warning("El bloque de validacion y supervisado no termino correctamente.")
  }
} else {
  warning("No se encontro scripts/run_validation_and_supervised.R; se omite validacion supervisada.")
}

# Step 10: advanced diagnostics block
diag_script <- file.path(project_root, "scripts", "run_diagnostics_block.R")
if (file.exists(diag_script)) {
  diag_cmd <- sprintf("Rscript %s", shQuote(diag_script))
  diag_status <- system(diag_cmd)
  if (diag_status != 0) {
    warning("El bloque de diagnosticos avanzados no termino correctamente.")
  }
} else {
  warning("No se encontro scripts/run_diagnostics_block.R; se omite bloque avanzado.")
}

# Step 11: coverage + axis-similarity block
cov_script <- file.path(project_root, "scripts", "run_coverage_similarity_axes.R")
if (file.exists(cov_script)) {
  cov_cmd <- sprintf("Rscript %s", shQuote(cov_script))
  cov_status <- system(cov_cmd)
  if (cov_status != 0) {
    warning("El bloque de cobertura y red por ejes no termino correctamente.")
  }
} else {
  warning("No se encontro scripts/run_coverage_similarity_axes.R; se omite bloque cobertura/red.")
}

# Step 12: benchmark externo + tablero KPI block
bench_script <- file.path(project_root, "scripts", "run_benchmark_kpi_block.R")
if (file.exists(bench_script)) {
  bench_cmd <- sprintf("Rscript %s", shQuote(bench_script))
  bench_status <- system(bench_cmd)
  if (bench_status != 0) {
    warning("El bloque benchmark + KPIs no termino correctamente.")
  }
} else {
  warning("No se encontro scripts/run_benchmark_kpi_block.R; se omite benchmark/KPIs.")
}

# Step 13: BART implementability block
bart_script <- file.path(project_root, "scripts", "run_bart_implementability.R")
if (file.exists(bart_script)) {
  bart_cmd <- sprintf("Rscript %s", shQuote(bart_script))
  bart_status <- system(bart_cmd)
  if (bart_status != 0) {
    warning("El bloque BART implementabilidad no termino correctamente.")
  }
} else {
  warning("No se encontro scripts/run_bart_implementability.R; se omite BART.")
}

# Step 14: render Quarto site
render_cmd <- sprintf("quarto render %s", shQuote(site_dir))
render_status <- system(render_cmd)
if (render_status != 0) {
  warning("No se pudo renderizar el sitio Quarto automaticamente.")
}

# Step 15: snapshot renv lockfile
try({
  renv::snapshot(prompt = FALSE, force = TRUE)
}, silent = TRUE)

message("Pipeline completado.")
message(glue("Propuestas: {nrow(propuestas)}"))
message(glue("Partidos: {n_distinct(propuestas$party)}"))
message(glue("Salida principal: {file.path(outputs_dir, 'propuestas.parquet')}"))
