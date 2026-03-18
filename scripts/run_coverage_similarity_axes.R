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

pkgs <- c("arrow", "dplyr", "tidyr", "purrr", "stringr", "readr", "tibble", "Matrix")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(Matrix)
})

set.seed(20260302)

propuestas_path <- file.path(outputs_dir, "propuestas_supervised.parquet")
embeddings_path <- file.path(outputs_dir, "embeddings.parquet")

if (!file.exists(propuestas_path)) stop("Falta outputs/propuestas_supervised.parquet", call. = FALSE)
if (!file.exists(embeddings_path)) stop("Falta outputs/embeddings.parquet", call. = FALSE)

propuestas <- read_parquet(propuestas_path)
embeddings <- read_parquet(embeddings_path)

# -------------------------------------------------------------------
# 1) Cobertura poblacional y territorial
# -------------------------------------------------------------------

population_dict <- list(
  ninos_ninas = c("niño", "niña", "ninez", "infancia", "adolescente"),
  jovenes = c("joven", "juventud", "estudiante"),
  mujeres = c("mujer", "madre", "violencia de genero"),
  adulto_mayor = c("adulto mayor", "tercera edad", "anciano"),
  discapacidad = c("discapacidad", "discapacitado", "inclusion"),
  trabajadores = c("trabajador", "laboral", "empleado", "sindicato"),
  mype_empresas = c("mype", "microempresa", "pyme", "empresa"),
  agricultores = c("agricultor", "campesino", "agro", "productor agrario"),
  pueblos_indigenas = c("indigena", "nativo", "comunidad campesina", "pueblo originario"),
  policia_ffaa = c("policia", "pnp", "ffaa", "fuerzas armadas")
)

territory_dict <- list(
  nacional = c("peru", "nacional", "pais"),
  lima_callao = c("lima", "callao", "metropolitana"),
  costa = c("costa", "litoral"),
  sierra = c("sierra", "andino", "altoandin"),
  selva_amazonia = c("selva", "amazonia", "amazonic"),
  regiones_provincias = c("region", "regional", "provincia", "departamento"),
  distritos_local = c("distrito", "municipal", "local"),
  rural = c("rural", "campo", "centro poblado"),
  urbano = c("urbano", "ciudad"),
  frontera = c("frontera", "fronterizo")
)

normalize_for_match <- function(txt) {
  txt <- enc2utf8(txt)
  txt <- str_to_lower(txt)
  txt <- iconv(txt, from = "UTF-8", to = "ASCII//TRANSLIT")
  txt <- str_replace_all(txt, "[^a-z0-9 ]+", " ")
  txt <- str_replace_all(txt, "\\s+", " ")
  str_trim(txt)
}

contains_any <- function(text, keys) {
  if (is.na(text) || text == "") return(FALSE)
  any(map_lgl(keys, ~ str_detect(text, fixed(normalize_for_match(.x)))))
}

prop_match <- propuestas |>
  transmute(
    party,
    doc_id,
    proposal_id,
    axis_supervised,
    instrument_supervised,
    proposal_text,
    source_snippet,
    txt_norm = normalize_for_match(coalesce(proposal_text, ""))
  )

population_long <- map_dfr(names(population_dict), function(group_name) {
  keys <- population_dict[[group_name]]
  tibble(
    party = prop_match$party,
    doc_id = prop_match$doc_id,
    proposal_id = prop_match$proposal_id,
    population_group = group_name,
    mentions_group = map_lgl(prop_match$txt_norm, ~ contains_any(.x, keys)),
    source_snippet = prop_match$source_snippet
  )
})

territory_long <- map_dfr(names(territory_dict), function(group_name) {
  keys <- territory_dict[[group_name]]
  tibble(
    party = prop_match$party,
    doc_id = prop_match$doc_id,
    proposal_id = prop_match$proposal_id,
    territory_group = group_name,
    mentions_group = map_lgl(prop_match$txt_norm, ~ contains_any(.x, keys)),
    source_snippet = prop_match$source_snippet
  )
})

write_parquet(population_long, file.path(outputs_dir, "coverage_population.parquet"))
write_csv(population_long, file.path(outputs_dir, "coverage_population.csv"))

write_parquet(territory_long, file.path(outputs_dir, "coverage_territory.parquet"))
write_csv(territory_long, file.path(outputs_dir, "coverage_territory.csv"))

pop_party <- population_long |>
  group_by(party, population_group) |>
  summarise(
    proposals_n = n(),
    covered_n = sum(mentions_group, na.rm = TRUE),
    coverage_share = mean(mentions_group, na.rm = TRUE),
    .groups = "drop"
  )

terr_party <- territory_long |>
  group_by(party, territory_group) |>
  summarise(
    proposals_n = n(),
    covered_n = sum(mentions_group, na.rm = TRUE),
    coverage_share = mean(mentions_group, na.rm = TRUE),
    .groups = "drop"
  )

coverage_party_summary <- propuestas |>
  distinct(party, proposal_id) |>
  count(party, name = "proposals_total") |>
  left_join(
    pop_party |>
      group_by(party) |>
      summarise(
        pop_groups_covered_n = sum(coverage_share > 0),
        pop_coverage_avg = mean(coverage_share),
        pop_coverage_min = min(coverage_share),
        .groups = "drop"
      ),
    by = "party"
  ) |>
  left_join(
    terr_party |>
      group_by(party) |>
      summarise(
        terr_groups_covered_n = sum(coverage_share > 0),
        terr_coverage_avg = mean(coverage_share),
        terr_coverage_min = min(coverage_share),
        .groups = "drop"
      ),
    by = "party"
  ) |>
  arrange(desc(pop_coverage_avg + terr_coverage_avg))

write_parquet(coverage_party_summary, file.path(outputs_dir, "coverage_party_summary.parquet"))
write_csv(coverage_party_summary, file.path(outputs_dir, "coverage_party_summary.csv"))

coverage_blindspots_party <- bind_rows(
  pop_party |>
    transmute(party, dim = "population", group = population_group, coverage_share),
  terr_party |>
    transmute(party, dim = "territory", group = territory_group, coverage_share)
) |>
  mutate(
    blindspot_flag = case_when(
      coverage_share == 0 ~ "sin_cobertura",
      coverage_share < 0.03 ~ "muy_baja",
      coverage_share < 0.08 ~ "baja",
      TRUE ~ "cobertura_ok"
    )
  )

write_parquet(coverage_blindspots_party, file.path(outputs_dir, "coverage_blindspots_party.parquet"))
write_csv(coverage_blindspots_party, file.path(outputs_dir, "coverage_blindspots_party.csv"))

# -------------------------------------------------------------------
# 2) Red de similitud por ejes
# -------------------------------------------------------------------

emb <- propuestas |>
  select(proposal_id, party, axis_supervised, proposal_text, concreteness_score) |>
  mutate(
    txt_norm = normalize_for_match(coalesce(proposal_text, "")),
    axis_network = case_when(
      str_detect(txt_norm, "corrup|coima|soborno|lavado de activos|transparen|integridad publica|contralor") ~ "corrupcion",
      str_detect(txt_norm, "transporte|movilidad|metro|tren|ferro|carretera|via|puerto|aeropuerto|terminal") ~ "transporte",
      str_detect(txt_norm, "salud|hospital|medic|essalud|vacuna|atencion primaria") ~ "salud",
      str_detect(txt_norm, "educacion|colegio|universidad|docente|curriculo|beca|aprendizaje") ~ "educacion",
      str_detect(txt_norm, "empleo|trabajo|laboral|salario|formalizacion|desempleo") ~ "empleo",
      axis_supervised %in% c("seguridad", "economia", "energia", "ambiente", "social", "infraestructura", "institucionalidad") ~ axis_supervised,
      str_detect(txt_norm, "agua|clima|ambient|residuo|forest|sostenible") ~ "ambiente",
      str_detect(txt_norm, "vivienda|saneamiento|agua potable|alcantarillado|obra publica") ~ "infraestructura",
      str_detect(txt_norm, "pobreza|inclusion|ninez|infancia|mujer|joven|adulto mayor|discapacidad") ~ "social",
      TRUE ~ "otros"
    )
  ) |>
  select(-proposal_text) |>
  left_join(embeddings, by = c("proposal_id", "party"))

dim_cols <- names(emb)[str_detect(names(emb), "^dim_")]
if (length(dim_cols) < 2) stop("Embeddings con dimensiones insuficientes.", call. = FALSE)

axis_party_centroids <- emb |>
  filter(!is.na(axis_network), axis_network != "") |>
  group_by(axis_network, party) |>
  filter(n() >= 3) |>
  summarise(
    proposals_n = n(),
    avg_concreteness = mean(concreteness_score, na.rm = TRUE),
    across(all_of(dim_cols), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

cosine <- function(a, b) {
  na <- sqrt(sum(a * a))
  nb <- sqrt(sum(b * b))
  if (na == 0 || nb == 0) return(0)
  sum(a * b) / (na * nb)
}

axis_levels <- sort(unique(axis_party_centroids$axis_network))

edges <- map_dfr(axis_levels, function(ax) {
  df <- axis_party_centroids |>
    filter(axis_network == ax)

  if (nrow(df) < 2) return(tibble())

  cmb <- t(combn(seq_len(nrow(df)), 2))
  sim <- map_dbl(seq_len(nrow(cmb)), function(i) {
    i1 <- cmb[i, 1]
    i2 <- cmb[i, 2]
    a <- as.numeric(df[i1, dim_cols, drop = TRUE])
    b <- as.numeric(df[i2, dim_cols, drop = TRUE])
    cosine(a, b)
  })

  tibble(
    axis = ax,
    party_a = df$party[cmb[, 1]],
    party_b = df$party[cmb[, 2]],
    proposals_a = df$proposals_n[cmb[, 1]],
    proposals_b = df$proposals_n[cmb[, 2]],
    avg_concreteness_a = df$avg_concreteness[cmb[, 1]],
    avg_concreteness_b = df$avg_concreteness[cmb[, 2]],
    similarity = sim
  ) |>
    mutate(
      edge_flag = case_when(
        similarity >= 0.90 ~ "muy_alta",
        similarity >= 0.82 ~ "alta",
        similarity >= 0.72 ~ "media",
        TRUE ~ "baja"
      )
    ) |>
    filter(similarity >= 0.72)
})

nodes <- axis_party_centroids |>
  transmute(
    axis = axis_network,
    party,
    proposals_n,
    avg_concreteness,
    node_size = pmax(8, round(sqrt(proposals_n) * 3, 0))
  )

write_parquet(edges, file.path(outputs_dir, "axis_similarity_edges.parquet"))
write_csv(edges, file.path(outputs_dir, "axis_similarity_edges.csv"))

write_parquet(nodes, file.path(outputs_dir, "axis_similarity_nodes.parquet"))
write_csv(nodes, file.path(outputs_dir, "axis_similarity_nodes.csv"))

axis_similarity_summary <- edges |>
  group_by(axis) |>
  summarise(
    edges_n = n(),
    mean_similarity = mean(similarity, na.rm = TRUE),
    max_similarity = max(similarity, na.rm = TRUE),
    parties_connected_n = n_distinct(c(party_a, party_b)),
    .groups = "drop"
  ) |>
  arrange(desc(mean_similarity), desc(edges_n))

write_parquet(axis_similarity_summary, file.path(outputs_dir, "axis_similarity_summary.parquet"))
write_csv(axis_similarity_summary, file.path(outputs_dir, "axis_similarity_summary.csv"))

# Overall summary
coverage_similarity_summary <- tibble(
  metric = c(
    "population_groups",
    "territory_groups",
    "coverage_blindspots_total",
    "axis_network_edges",
    "axis_network_axes_with_edges"
  ),
  value = c(
    n_distinct(population_long$population_group),
    n_distinct(territory_long$territory_group),
    sum(coverage_blindspots_party$blindspot_flag %in% c("sin_cobertura", "muy_baja"), na.rm = TRUE),
    nrow(edges),
    n_distinct(edges$axis)
  )
)

write_csv(coverage_similarity_summary, file.path(outputs_dir, "coverage_similarity_summary.csv"))

message("Bloque cobertura + red por ejes completado:")
message(sprintf(" - Cobertura poblacional: %d filas", nrow(population_long)))
message(sprintf(" - Cobertura territorial: %d filas", nrow(territory_long)))
message(sprintf(" - Ejes con red: %d", n_distinct(edges$axis)))
message(sprintf(" - Aristas de similitud por eje: %d", nrow(edges)))
