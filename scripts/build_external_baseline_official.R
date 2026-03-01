#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
external_dir <- file.path(project_root, "data", "external")
tmp_dir <- file.path(project_root, "outputs", "tmp")

if (!dir.exists(external_dir)) dir.create(external_dir, recursive = TRUE)
if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

pkgs <- c("dplyr", "readr", "stringr", "stringi", "purrr", "tibble", "jsonlite")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(purrr)
  library(tibble)
  library(jsonlite)
})

set.seed(20260305)

metadata_url <- "https://estadisticas.bcrp.gob.pe/estadisticas/series/metadata"
metadata_raw <- file.path(tmp_dir, "bcrp_metadata_raw.csv")

tryCatch({
  download.file(metadata_url, destfile = metadata_raw, mode = "wb", quiet = TRUE)
}, error = function(e) {
  stop(sprintf("No se pudo descargar metadata BCRP: %s", conditionMessage(e)), call. = FALSE)
})

md <- read_delim(metadata_raw, delim = ";", show_col_types = FALSE, locale = locale(encoding = "latin1"))
names(md) <- make.names(names(md))

extract_year <- function(period_name) {
  if (is.na(period_name) || period_name == "") return(NA_integer_)

  y4 <- str_extract(period_name, "(19|20)\\d{2}")
  if (!is.na(y4)) return(as.integer(y4))

  y2 <- str_match(period_name, "\\.(\\d{2})$")[, 2]
  if (!is.na(y2)) return(2000L + as.integer(y2))

  NA_integer_
}

fetch_series_snapshot <- function(series_code, prefer_year = 2025L) {
  api_url <- sprintf("https://estadisticas.bcrp.gob.pe/estadisticas/series/api/%s/json", series_code)

  js <- tryCatch(fromJSON(api_url), error = function(e) NULL)
  if (is.null(js) || is.null(js$periods) || nrow(js$periods) == 0) {
    return(tibble(
      series_code = series_code,
      series_name_api = NA_character_,
      baseline_period = NA_character_,
      baseline_year = NA_integer_,
      baseline_value = NA_real_,
      source_url_api = api_url
    ))
  }

  periods <- as_tibble(js$periods) |>
    transmute(
      baseline_period = as.character(name),
      baseline_value = suppressWarnings(as.numeric(values)),
      baseline_year = map_int(baseline_period, extract_year)
    ) |>
    filter(!is.na(baseline_value))

  if (nrow(periods) == 0) {
    return(tibble(
      series_code = series_code,
      series_name_api = js$config$series$name[[1]],
      baseline_period = NA_character_,
      baseline_year = NA_integer_,
      baseline_value = NA_real_,
      source_url_api = api_url
    ))
  }

  chosen <- periods |>
    filter(!is.na(baseline_year), baseline_year <= prefer_year) |>
    arrange(baseline_year, row_number())

  if (nrow(chosen) == 0) {
    chosen <- periods |>
      filter(!is.na(baseline_year)) |>
      arrange(baseline_year, row_number())
  }

  chosen <- chosen |> slice_tail(n = 1)

  tibble(
    series_code = series_code,
    series_name_api = js$config$series$name[[1]],
    baseline_period = chosen$baseline_period,
    baseline_year = as.integer(chosen$baseline_year),
    baseline_value = as.numeric(chosen$baseline_value),
    source_url_api = api_url
  )
}

# Indicadores seleccionados con extracción automática oficial (BCRP API + metadatos fuente INEI/MEF)
indicator_spec <- tribble(
  ~indicator_id, ~series_code, ~indicator_name, ~axis, ~source_institution, ~unit, ~better_direction, ~target_2031, ~keyword_regex, ~plausible_min, ~plausible_max, ~kpi_formula, ~frequency, ~target_note,
  "eco_inflacion_ipc_mensual", "PN01271PM", "IPC Lima Metropolitana (var. % mensual)", "economia", "INEI/BCRP", "porcentaje", "down", 0.15, "inflacion|ipc|precios|costo de vida", -2, 5, "Variacion porcentual mensual del IPC de Lima Metropolitana", "mensual", "Supuesto analitico interno",
  "eco_pbi_var_interanual", "PN01728AM", "PBI (var. % interanual)", "economia", "INEI/BCRP", "porcentaje", "up", 4.50, "pbi|crecimiento|actividad economica|produccion", -20, 20, "Variacion porcentual interanual del PBI", "mensual", "Supuesto analitico interno",
  "emp_desempleo_lima", "PN38063GM", "Tasa de desempleo Lima Metropolitana (%)", "empleo", "INEI/BCRP", "porcentaje", "down", 4.50, "desempleo|empleo|mercado laboral", 0, 25, "Desocupados / PEA * 100", "mensual", "Supuesto analitico interno",
  "emp_informalidad_ano_movil", "PN38071GM", "Tasa de empleo informal - ano movil (%)", "empleo", "INEI/BCRP", "porcentaje", "down", 60.0, "informalidad|formalizacion|empleo informal|mype", 0, 100, "Ocupados informales / ocupados totales * 100", "mensual", "Supuesto analitico interno",
  "inst_resultado_spnf_pbi", "PN02460FQ", "Resultado economico del SPNF (% del PBI)", "institucionalidad", "MEF/BCRP", "porcentaje", "up", -1.00, "deficit fiscal|resultado economico|spnf|regla fiscal", -15, 10, "Resultado economico SPNF como porcentaje del PBI", "trimestral", "Supuesto analitico interno",
  "inst_deuda_publica_externa_pbi", "PN02465FQ", "Saldo de deuda publica externa (% del PBI)", "institucionalidad", "MEF/BCRP", "porcentaje", "down", 10.0, "deuda publica|deuda externa|sostenibilidad fiscal", 0, 80, "Saldo deuda publica externa / PBI * 100", "trimestral", "Supuesto analitico interno",
  "inst_resultado_spnf_pbi_anual", "PM05780FA", "Resultado economico del SPNF (% del PBI) - anual", "institucionalidad", "MEF/BCRP", "porcentaje", "up", -1.00, "deficit fiscal|resultado economico|spnf|regla fiscal", -15, 10, "Resultado economico SPNF anual como porcentaje del PBI", "anual", "Supuesto analitico interno",
  "inst_resultado_gg_pbi_anual", "PM05817FA", "Resultado economico del Gobierno General (% del PBI) - anual", "institucionalidad", "MEF/BCRP", "porcentaje", "up", -1.20, "deficit fiscal|gobierno general|resultado economico", -15, 10, "Resultado economico del gobierno general / PBI * 100", "anual", "Supuesto analitico interno",
  "inst_resultado_gc_pbi_anual", "PM05848FA", "Resultado economico del Gobierno Central (% del PBI) - anual", "institucionalidad", "MEF/BCRP", "porcentaje", "up", -1.50, "deficit fiscal|gobierno central|resultado economico", -15, 10, "Resultado economico del gobierno central / PBI * 100", "anual", "Supuesto analitico interno",
  "inst_resultado_gl_pbi_anual", "PM05970FA", "Resultado economico de Gobiernos Locales (% del PBI) - anual", "institucionalidad", "MEF/BCRP", "porcentaje", "up", 0.30, "gobiernos locales|resultado economico|descentralizacion fiscal", -5, 5, "Resultado economico gobiernos locales / PBI * 100", "anual", "Supuesto analitico interno",
  "inf_gasto_capital_gg", "PM05909FA", "Gasto de capital del Gobierno General (millones S/)", "infraestructura", "MEF/BCRP", "millones_soles", "up", 90000, "inversion publica|gasto de capital|infraestructura|obra publica", 0, 300000, "Suma anual de gasto de capital del gobierno general", "anual", "Supuesto analitico interno",
  "ene_pbi_electricidad_agua_var", "PM04978AA", "Electricidad y agua (var. % interanual)", "energia", "INEI/BCRP", "porcentaje", "up", 3.00, "energia|electricidad|agua|servicios basicos", -20, 20, "Variacion porcentual interanual del PBI sector electricidad y agua", "anual", "Supuesto analitico interno",
  "ene_pbi_electricidad_agua_nivel", "PM04996AA", "Electricidad y agua (millones S/ de 2007)", "energia", "INEI/BCRP", "millones_soles_2007", "up", 14000, "energia|electricidad|agua|servicios basicos", 0, 40000, "Nivel anual del PBI sector electricidad y agua", "anual", "Supuesto analitico interno"
)

snapshots <- map_dfr(indicator_spec$series_code, fetch_series_snapshot, prefer_year = 2025L)

md_sel <- md |>
  select(Código.de.serie, Grupo.de.serie, Nombre.de.serie, Fuente, Frecuencia, Fecha.de.actualización, Fecha.de.fin) |>
  rename(
    series_code = Código.de.serie,
    metadata_group = Grupo.de.serie,
    metadata_series_name = Nombre.de.serie,
    metadata_source = Fuente,
    metadata_frequency = Frecuencia,
    metadata_updated = Fecha.de.actualización,
    metadata_end = Fecha.de.fin
  )

baseline <- indicator_spec |>
  left_join(snapshots, by = "series_code") |>
  left_join(md_sel, by = "series_code") |>
  mutate(
    source_series = if_else(!is.na(series_name_api), series_name_api, metadata_series_name),
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value),
    target_2031 = as.numeric(target_2031),
    plausible_min = as.numeric(plausible_min),
    plausible_max = as.numeric(plausible_max),
    better_direction = str_to_lower(better_direction),
    baseline_value = round(baseline_value, 6),
    source_url_metadata = metadata_url,
    source_kind = "official_bcrp_api",
    retrieval_date = as.character(Sys.Date())
  ) |>
  select(
    indicator_id, indicator_name, axis,
    source_institution, source_series,
    baseline_year, baseline_value, target_2031,
    unit, better_direction,
    keyword_regex, plausible_min, plausible_max,
    kpi_formula, frequency,
    series_code, baseline_period,
    source_url_api, source_url_metadata,
    metadata_source, metadata_group, metadata_frequency, metadata_updated,
    target_note, source_kind, retrieval_date
  )

out_main <- baseline |>
  select(
    indicator_id, indicator_name, axis,
    source_institution, source_series,
    baseline_year, baseline_value, target_2031,
    unit, better_direction,
    keyword_regex, plausible_min, plausible_max,
    kpi_formula, frequency
  )

write_csv(out_main, file.path(external_dir, "linea_base_peru_2025.csv"))
write_csv(baseline, file.path(external_dir, "linea_base_peru_2025_sources.csv"))

message("Linea base oficial generada:")
message(sprintf(" - Indicadores: %d", nrow(out_main)))
message(sprintf(" - Archivo principal: %s", file.path(external_dir, "linea_base_peru_2025.csv")))
message(sprintf(" - Trazabilidad: %s", file.path(external_dir, "linea_base_peru_2025_sources.csv")))

missing_vals <- out_main |>
  filter(is.na(baseline_value) | is.na(baseline_year))

if (nrow(missing_vals) > 0) {
  warning(sprintf("Hay %d indicadores sin baseline_value/baseline_year. Revisar trazabilidad.", nrow(missing_vals)))
}
