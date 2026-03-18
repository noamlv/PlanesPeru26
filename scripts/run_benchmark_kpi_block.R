#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_dir <- file.path(project_root, "outputs")
data_external_dir <- file.path(project_root, "data", "external")

if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)
if (!dir.exists(data_external_dir)) dir.create(data_external_dir, recursive = TRUE)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

pkgs <- c("arrow", "dplyr", "tidyr", "purrr", "stringr", "stringi", "readr", "tibble")
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
})

set.seed(20260303)

propuestas_path <- file.path(outputs_dir, "propuestas_supervised.parquet")
if (!file.exists(propuestas_path)) stop("Falta outputs/propuestas_supervised.parquet", call. = FALSE)

propuestas <- read_parquet(propuestas_path)

normalize_text <- function(x) {
  x <- enc2utf8(coalesce(x, ""))
  x <- str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- str_replace_all(x, "[^a-z0-9 ]+", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

# -------------------------------------------------------------------
# 1) Benchmark externo (INEI/BCRP/MEF) con fallback proxy reproducible
# -------------------------------------------------------------------

required_ref_cols <- c(
  "indicator_id", "indicator_name", "axis", "source_institution", "source_series",
  "baseline_year", "baseline_value", "target_2031", "unit", "better_direction",
  "keyword_regex", "plausible_min", "plausible_max", "kpi_formula", "frequency"
)

fallback_reference <- tibble(
  indicator_id = c(
    "seg_homicidios", "seg_victimizacion", "eco_informalidad", "eco_inflacion",
    "sal_anemia", "edu_logro_matematica", "emp_formalidad", "inst_corrupcion",
    "inf_agua_rural", "amb_deforestacion", "ene_renovables", "soc_pobreza"
  ),
  indicator_name = c(
    "Tasa de homicidios por 100 mil hab.",
    "Victimizacion urbana (%)",
    "Informalidad laboral (%)",
    "Inflacion anual (%)",
    "Anemia infantil 6-35 meses (%)",
    "Escolares con logro satisfactorio en matematica (%)",
    "Empleo formal sobre empleo total (%)",
    "Percepcion de corrupcion en gestion publica (%)",
    "Hogares rurales con acceso a agua segura (%)",
    "Deforestacion anual (ha)",
    "Participacion de renovables en matriz electrica (%)",
    "Pobreza monetaria (%)"
  ),
  axis = c(
    "seguridad", "seguridad", "economia", "economia", "salud", "educacion",
    "empleo", "institucionalidad", "infraestructura", "ambiente", "energia", "social"
  ),
  source_institution = c(
    "INEI", "INEI", "INEI", "BCRP", "INEI", "MINEDU/INEI", "INEI", "PCM/INEI",
    "INEI", "MINAM", "MINEM/OSINERGMIN", "INEI"
  ),
  source_series = c(
    "ENAHO/Seguridad", "ENAPRES", "ENAHO", "BCRP inflacion", "ENDES", "ECE/UMC",
    "ENAHO", "Encuesta de percepcion", "ENAHO", "Geobosques", "Balance energetico", "ENAHO"
  ),
  baseline_year = c(2024, 2024, 2024, 2025, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
  baseline_value = c(8.1, 27.0, 71.0, 2.4, 42.0, 32.0, 29.0, 62.0, 74.0, 155000, 9.0, 29.0),
  target_2031 = c(5.0, 20.0, 55.0, 2.0, 25.0, 50.0, 42.0, 45.0, 86.0, 100000, 20.0, 18.0),
  unit = c(
    "por_100k", "porcentaje", "porcentaje", "porcentaje", "porcentaje", "porcentaje",
    "porcentaje", "porcentaje", "porcentaje", "hectareas", "porcentaje", "porcentaje"
  ),
  better_direction = c(
    "down", "down", "down", "down", "down", "up", "up", "down", "up", "down", "up", "down"
  ),
  keyword_regex = c(
    "homicid|asesinat|crimen violento|sicariato",
    "victimiz|delincu|inseguridad ciudadana",
    "informal|formaliz|mype|laboral",
    "inflacion|precio|ipc|costo de vida",
    "anemia|desnutric|hierro|primera infancia",
    "aprendiz|matemat|lectura|colegio|escuela",
    "empleo formal|planilla|formaliz|laboral",
    "corrup|transparen|integridad|control",
    "agua segura|saneamiento|rural|potable",
    "deforest|bosque|tala",
    "renovable|solar|eolica|transicion energetica",
    "pobreza|ingreso|vulnerabilidad"
  ),
  plausible_min = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 50000, 0, 0),
  plausible_max = c(40, 100, 100, 20, 100, 100, 100, 100, 100, 300000, 100, 100),
  kpi_formula = c(
    "(Homicidios reportados / Poblacion) * 100000",
    "Victimas de delito / Poblacion urbana * 100",
    "Ocupados informales / Ocupados totales * 100",
    "Variacion anual del IPC",
    "Ninos 6-35 meses con anemia / total ninos 6-35 meses * 100",
    "Estudiantes con logro satisfactorio / total evaluados * 100",
    "Ocupados formales / Ocupados totales * 100",
    "Personas que perciben alta corrupcion / total encuestados * 100",
    "Hogares rurales con agua segura / total hogares rurales * 100",
    "Hectareas deforestadas en el anio",
    "Generacion renovable / generacion electrica total * 100",
    "Poblacion bajo linea de pobreza / total poblacion * 100"
  ),
  frequency = c(
    "anual", "anual", "trimestral", "mensual", "anual", "anual",
    "trimestral", "anual", "anual", "anual", "anual", "trimestral"
  )
)

user_ref_path <- file.path(data_external_dir, "linea_base_peru_2025.csv")
proxy_ref_path <- file.path(data_external_dir, "linea_base_peru_proxy.csv")

if (file.exists(user_ref_path)) {
  ref <- suppressMessages(read_csv(user_ref_path, show_col_types = FALSE))
  missing_cols <- setdiff(required_ref_cols, names(ref))
  if (length(missing_cols) > 0) {
    warning(sprintf("La referencia externa local no tiene columnas requeridas: %s. Se usa fallback proxy.", paste(missing_cols, collapse = ", ")))
    ref <- fallback_reference
    write_csv(ref, proxy_ref_path)
    reference_source <- "proxy_local"
  } else {
    ref <- ref |>
      mutate(
        baseline_year = as.integer(baseline_year),
        baseline_value = as.numeric(baseline_value),
        target_2031 = as.numeric(target_2031),
        plausible_min = as.numeric(plausible_min),
        plausible_max = as.numeric(plausible_max)
      )
    reference_source <- "user_external"
  }
} else {
  ref <- fallback_reference
  write_csv(ref, proxy_ref_path)
  reference_source <- "proxy_local"
}

ref <- ref |>
  mutate(
    axis = str_to_lower(axis),
    better_direction = str_to_lower(better_direction),
    source_kind = reference_source
  )

write_parquet(ref, file.path(outputs_dir, "benchmark_indicator_reference.parquet"))
write_csv(ref, file.path(outputs_dir, "benchmark_indicator_reference.csv"))

# Proposal -> indicator mapping
props <- propuestas |>
  transmute(
    party, doc_id, proposal_id,
    axis_supervised = str_to_lower(coalesce(axis, axis_supervised, "otros")),
    instrument_supervised,
    has_quant_target,
    has_time_horizon,
    mentions_funding_source,
    mentions_cost,
    concreteness_score,
    proposal_text,
    numbers_found,
    source_snippet,
    text_norm = normalize_text(proposal_text)
  )

# Fast numeric parser with mixed decimal/group separators
parse_number_token <- function(token) {
  tok <- str_trim(token)
  if (tok == "" || is.na(tok)) return(NA_real_)

  p1 <- suppressWarnings(parse_number(tok, locale = locale(decimal_mark = ",", grouping_mark = ".")))
  if (!is.na(p1)) return(as.numeric(p1))

  p2 <- suppressWarnings(parse_number(tok, locale = locale(decimal_mark = ".", grouping_mark = ",")))
  if (!is.na(p2)) return(as.numeric(p2))

  NA_real_
}

extract_numeric_candidates <- function(text, numbers_found) {
  raw <- c()
  if (!is.na(numbers_found) && str_trim(numbers_found) != "") {
    raw <- c(raw, unlist(str_split(numbers_found, ";", simplify = FALSE), use.names = FALSE))
  }
  if (!is.na(text) && str_trim(text) != "") {
    raw <- c(raw, str_extract_all(text, "\\b\\d+[\\d\\.,]*%?\\b")[[1]])
  }
  raw <- unique(str_trim(raw))
  vals <- map_dbl(raw, parse_number_token)
  vals <- vals[is.finite(vals)]
  unique(vals)
}

candidates <- props |>
  left_join(ref, by = c("axis_supervised" = "axis"), relationship = "many-to-many") |>
  mutate(
    keyword_hits = if_else(
      is.na(keyword_regex) | keyword_regex == "",
      0,
      str_count(text_norm, regex(keyword_regex, ignore_case = TRUE))
    ),
    axis_match = !is.na(indicator_id),
    map_score = keyword_hits + if_else(axis_match, 0.5, 0)
  )

# Fallback to all indicators when axis has no direct indicator
no_axis_rows <- candidates |>
  filter(!axis_match) |>
  select(names(props))

if (nrow(no_axis_rows) > 0) {
  cross_any <- tidyr::crossing(no_axis_rows, ref) |>
    mutate(
      keyword_hits = str_count(text_norm, regex(keyword_regex, ignore_case = TRUE)),
      axis_match = FALSE,
      map_score = keyword_hits
    )
  candidates <- bind_rows(candidates |> filter(axis_match), cross_any)
}

mapping <- candidates |>
  group_by(proposal_id) |>
  arrange(desc(map_score), desc(keyword_hits), indicator_id, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(map_quality = case_when(
    map_score >= 3 ~ "alta",
    map_score >= 1 ~ "media",
    TRUE ~ "baja"
  ))

mapping <- mapping |>
  rowwise() |>
  mutate(
    numeric_candidates = list(extract_numeric_candidates(proposal_text, numbers_found)),
    proposal_target_value = {
      vals <- numeric_candidates
      vals <- vals[vals >= plausible_min & vals <= plausible_max]
      if (length(vals) == 0) NA_real_ else vals[[1]]
    },
    target_extraction_source = case_when(
      is.na(proposal_target_value) ~ "none",
      !is.na(numbers_found) & str_detect(numbers_found, "\\d") ~ "numbers_found",
      TRUE ~ "proposal_text"
    )
  ) |>
  ungroup()

benchmark_gap <- mapping |>
  mutate(
    gap_baseline_to_target = abs(target_2031 - baseline_value),
    direction_consistent = case_when(
      is.na(proposal_target_value) ~ NA,
      better_direction == "up" ~ proposal_target_value >= baseline_value,
      better_direction == "down" ~ proposal_target_value <= baseline_value,
      TRUE ~ NA
    ),
    gap_proposal_to_target = case_when(
      is.na(proposal_target_value) ~ NA_real_,
      better_direction == "up" ~ target_2031 - proposal_target_value,
      better_direction == "down" ~ proposal_target_value - target_2031,
      TRUE ~ NA_real_
    ),
    gap_abs_ratio = case_when(
      is.na(gap_proposal_to_target) ~ NA_real_,
      gap_baseline_to_target == 0 ~ 0,
      TRUE ~ abs(gap_proposal_to_target) / gap_baseline_to_target
    ),
    benchmark_alignment_score = case_when(
      is.na(proposal_target_value) ~ pmax(10, round(coalesce(concreteness_score, 0) * 0.40, 1)),
      !isTRUE(direction_consistent) ~ pmax(5, round(20 + coalesce(concreteness_score, 0) * 0.25, 1)),
      TRUE ~ pmin(
        100,
        round(
          100 - pmin(80, coalesce(gap_abs_ratio, 1) * 60) +
            if_else(has_time_horizon, 10, 0) +
            if_else(mentions_funding_source | mentions_cost, 10, 0),
          1
        )
      )
    ),
    benchmark_status = case_when(
      is.na(proposal_target_value) ~ "sin_meta_cuantificada",
      !isTRUE(direction_consistent) ~ "direccion_inconsistente",
      gap_proposal_to_target > 0 ~ "por_debajo_meta_2031",
      gap_proposal_to_target <= 0 ~ "alcanza_o_supera_meta_2031",
      TRUE ~ "indeterminado"
    )
  ) |>
  select(
    party, doc_id, proposal_id, axis_supervised,
    instrument_supervised,
    indicator_id, indicator_name, source_institution, source_series,
    baseline_year, baseline_value, target_2031, unit, better_direction,
    map_quality, keyword_hits,
    proposal_target_value, target_extraction_source,
    gap_baseline_to_target, gap_proposal_to_target, gap_abs_ratio,
    direction_consistent, benchmark_alignment_score, benchmark_status,
    has_quant_target, has_time_horizon, mentions_cost, mentions_funding_source,
    concreteness_score, source_snippet
  )

write_parquet(benchmark_gap, file.path(outputs_dir, "benchmark_gap_proposal.parquet"))
write_csv(benchmark_gap, file.path(outputs_dir, "benchmark_gap_proposal.csv"))

benchmark_party_axis <- benchmark_gap |>
  group_by(party, axis_supervised) |>
  summarise(
    proposals_n = n(),
    with_quant_target_n = sum(!is.na(proposal_target_value)),
    direction_consistent_pct = mean(direction_consistent, na.rm = TRUE),
    avg_alignment_score = mean(benchmark_alignment_score, na.rm = TRUE),
    median_gap_abs_ratio = median(gap_abs_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    direction_consistent_pct = if_else(is.nan(direction_consistent_pct), NA_real_, direction_consistent_pct),
    median_gap_abs_ratio = if_else(is.infinite(median_gap_abs_ratio), NA_real_, median_gap_abs_ratio)
  )

write_parquet(benchmark_party_axis, file.path(outputs_dir, "benchmark_gap_party_axis.parquet"))
write_csv(benchmark_party_axis, file.path(outputs_dir, "benchmark_gap_party_axis.csv"))

benchmark_indicator_party <- benchmark_gap |>
  group_by(party, indicator_id, indicator_name, source_institution) |>
  summarise(
    proposals_n = n(),
    avg_alignment_score = mean(benchmark_alignment_score, na.rm = TRUE),
    pct_sin_meta = mean(benchmark_status == "sin_meta_cuantificada", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(proposals_n), desc(avg_alignment_score))

write_parquet(benchmark_indicator_party, file.path(outputs_dir, "benchmark_gap_indicator_party.parquet"))
write_csv(benchmark_indicator_party, file.path(outputs_dir, "benchmark_gap_indicator_party.csv"))

# -------------------------------------------------------------------
# 2) Tablero ex-post de KPIs verificables (2026-2031)
# -------------------------------------------------------------------

kpi_tracker <- benchmark_gap |>
  mutate(
    kpi_id = paste0("kpi_", str_pad(row_number(), width = 6, side = "left", pad = "0")),
    monitoring_window = "2026-2031",
    baseline_for_projection = baseline_value,
    milestone_2027 = baseline_value + (target_2031 - baseline_value) * ((2027 - baseline_year) / pmax(1, (2031 - baseline_year))),
    milestone_2029 = baseline_value + (target_2031 - baseline_value) * ((2029 - baseline_year) / pmax(1, (2031 - baseline_year))),
    milestone_2031 = target_2031,
    data_frequency = case_when(
      source_institution == "BCRP" ~ "mensual/trimestral",
      TRUE ~ "anual/trimestral"
    ),
    kpi_formula_guess = case_when(
      unit == "porcentaje" ~ "(Numerador / Denominador) * 100",
      unit == "por_100k" ~ "(Eventos / Poblacion) * 100000",
      unit == "hectareas" ~ "Suma anual de hectareas afectadas",
      TRUE ~ "Serie oficial reportada"
    ),
    verifiability_score = pmin(
      100,
      pmax(
        0,
        round(
          if_else(!is.na(proposal_target_value), 35, 10) +
            if_else(has_time_horizon, 20, 0) +
            if_else(mentions_funding_source | mentions_cost, 15, 0) +
            if_else(map_quality == "alta", 20, if_else(map_quality == "media", 12, 5)) +
            if_else(!is.na(source_institution) & source_institution != "", 10, 0) -
            if_else(is.na(instrument_supervised) | instrument_supervised == "unspecified", 15, 0),
          1
        )
      )
    ),
    verifiability_tier = case_when(
      verifiability_score >= 75 ~ "alta",
      verifiability_score >= 50 ~ "media",
      TRUE ~ "baja"
    ),
    is_kpi_verifiable = verifiability_tier %in% c("alta", "media")
  ) |>
  select(
    kpi_id, party, doc_id, proposal_id, axis_supervised,
    indicator_id, indicator_name, source_institution,
    monitoring_window, baseline_year, baseline_for_projection,
    milestone_2027, milestone_2029, milestone_2031,
    unit, better_direction, data_frequency, kpi_formula_guess,
    benchmark_alignment_score, verifiability_score, verifiability_tier,
    is_kpi_verifiable, source_snippet
  )

write_parquet(kpi_tracker, file.path(outputs_dir, "kpi_tracker_proposal.parquet"))
write_csv(kpi_tracker, file.path(outputs_dir, "kpi_tracker_proposal.csv"))

kpi_dashboard_party <- kpi_tracker |>
  group_by(party) |>
  summarise(
    proposals_kpi_n = n(),
    kpi_verificables_n = sum(is_kpi_verifiable, na.rm = TRUE),
    kpi_alta_n = sum(verifiability_tier == "alta", na.rm = TRUE),
    kpi_media_n = sum(verifiability_tier == "media", na.rm = TRUE),
    kpi_baja_n = sum(verifiability_tier == "baja", na.rm = TRUE),
    verificables_pct = mean(is_kpi_verifiable, na.rm = TRUE),
    avg_verifiability_score = mean(verifiability_score, na.rm = TRUE),
    avg_alignment_score = mean(benchmark_alignment_score, na.rm = TRUE),
    axis_coverage_n = n_distinct(axis_supervised),
    .groups = "drop"
  ) |>
  arrange(desc(verificables_pct), desc(avg_verifiability_score))

write_parquet(kpi_dashboard_party, file.path(outputs_dir, "kpi_dashboard_party.parquet"))
write_csv(kpi_dashboard_party, file.path(outputs_dir, "kpi_dashboard_party.csv"))

kpi_dashboard_axis <- kpi_tracker |>
  group_by(axis_supervised) |>
  summarise(
    proposals_kpi_n = n(),
    verificables_pct = mean(is_kpi_verifiable, na.rm = TRUE),
    avg_verifiability_score = mean(verifiability_score, na.rm = TRUE),
    avg_alignment_score = mean(benchmark_alignment_score, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_verifiability_score))

write_parquet(kpi_dashboard_axis, file.path(outputs_dir, "kpi_dashboard_axis.parquet"))
write_csv(kpi_dashboard_axis, file.path(outputs_dir, "kpi_dashboard_axis.csv"))

kpi_tracking_template <- kpi_tracker |>
  transmute(
    kpi_id, party, proposal_id, indicator_id, indicator_name,
    baseline_year,
    observed_2026 = NA_real_,
    observed_2027 = NA_real_,
    observed_2028 = NA_real_,
    observed_2029 = NA_real_,
    observed_2030 = NA_real_,
    observed_2031 = NA_real_,
    latest_update = NA_character_,
    source_url = NA_character_
  )

write_csv(kpi_tracking_template, file.path(outputs_dir, "kpi_tracking_template_2026_2031.csv"))

summary_tbl <- tibble(
  metric = c(
    "benchmark_reference_source",
    "benchmark_indicators_n",
    "benchmark_rows_proposal",
    "benchmark_avg_alignment_score",
    "kpi_tracker_rows",
    "kpi_verificables_pct",
    "kpi_verifiability_mean"
  ),
  value = c(
    reference_source,
    nrow(ref),
    nrow(benchmark_gap),
    round(mean(benchmark_gap$benchmark_alignment_score, na.rm = TRUE), 2),
    nrow(kpi_tracker),
    round(mean(kpi_tracker$is_kpi_verifiable, na.rm = TRUE) * 100, 2),
    round(mean(kpi_tracker$verifiability_score, na.rm = TRUE), 2)
  )
)

write_csv(summary_tbl, file.path(outputs_dir, "benchmark_kpi_summary.csv"))

message("Bloque benchmark + KPIs completado:")
message(sprintf(" - Fuente benchmark: %s", reference_source))
message(sprintf(" - Indicadores benchmark: %d", nrow(ref)))
message(sprintf(" - Filas benchmark por propuesta: %d", nrow(benchmark_gap)))
message(sprintf(" - KPI tracker filas: %d", nrow(kpi_tracker)))
