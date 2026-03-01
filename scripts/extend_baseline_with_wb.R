#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
external_dir <- file.path(project_root, "data", "external")

main_path <- file.path(external_dir, "linea_base_peru_2025.csv")
sources_path <- file.path(external_dir, "linea_base_peru_2025_sources.csv")

if (!file.exists(main_path) || !file.exists(sources_path)) {
  stop("Faltan linea_base_peru_2025.csv o linea_base_peru_2025_sources.csv. Ejecuta build_external_baseline_official.R primero.", call. = FALSE)
}

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) install.packages(missing, repos = "https://cloud.r-project.org")
}

pkgs <- c("dplyr", "readr", "tibble", "purrr", "jsonlite", "stringr")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(purrr)
  library(jsonlite)
  library(stringr)
})

set.seed(20260305)

base_main <- read_csv(main_path, show_col_types = FALSE)
base_src <- read_csv(sources_path, show_col_types = FALSE)
base_src <- base_src |>
  mutate(
    metadata_updated = as.character(metadata_updated),
    retrieval_date = as.character(retrieval_date)
  )

required_cols <- c(
  "indicator_id", "indicator_name", "axis", "source_institution", "source_series",
  "baseline_year", "baseline_value", "target_2031", "unit", "better_direction",
  "keyword_regex", "plausible_min", "plausible_max", "kpi_formula", "frequency"
)

missing_required <- setdiff(required_cols, names(base_main))
if (length(missing_required) > 0) {
  stop(sprintf("linea_base_peru_2025.csv no tiene columnas requeridas: %s", paste(missing_required, collapse = ", ")), call. = FALSE)
}

fetch_wb_latest <- function(indicator_code, country = "PER") {
  api_url <- sprintf("https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&per_page=200", country, indicator_code)
  payload <- tryCatch(fromJSON(api_url), error = function(e) NULL)

  if (is.null(payload) || length(payload) < 2 || is.null(payload[[2]])) {
    return(tibble(
      wb_indicator_code = indicator_code,
      source_series = NA_character_,
      baseline_year = NA_integer_,
      baseline_value = NA_real_,
      baseline_period = NA_character_,
      source_url_api = api_url,
      source_url_metadata = sprintf("https://api.worldbank.org/v2/indicator/%s?format=json", indicator_code)
    ))
  }

  data <- as_tibble(payload[[2]]) |>
    transmute(
      baseline_period = as.character(date),
      baseline_year = suppressWarnings(as.integer(date)),
      baseline_value = suppressWarnings(as.numeric(value)),
      source_series = as.character(indicator$value)
    ) |>
    filter(!is.na(baseline_value), !is.na(baseline_year)) |>
    arrange(desc(baseline_year))

  if (nrow(data) == 0) {
    return(tibble(
      wb_indicator_code = indicator_code,
      source_series = NA_character_,
      baseline_year = NA_integer_,
      baseline_value = NA_real_,
      baseline_period = NA_character_,
      source_url_api = api_url,
      source_url_metadata = sprintf("https://api.worldbank.org/v2/indicator/%s?format=json", indicator_code)
    ))
  }

  top <- data |> slice(1)

  tibble(
    wb_indicator_code = indicator_code,
    source_series = top$source_series,
    baseline_year = top$baseline_year,
    baseline_value = top$baseline_value,
    baseline_period = top$baseline_period,
    source_url_api = api_url,
    source_url_metadata = sprintf("https://api.worldbank.org/v2/indicator/%s?format=json", indicator_code)
  )
}

wb_spec <- tribble(
  ~indicator_id, ~wb_indicator_code, ~indicator_name, ~axis, ~target_2031, ~unit, ~better_direction, ~keyword_regex, ~plausible_min, ~plausible_max, ~kpi_formula, ~frequency, ~target_note,
  "seg_homicidios_total_wb", "VC.IHR.PSRC.P5", "Homicidios intencionales (por 100 mil hab.)", "seguridad", 6.0, "por_100k", "down", "homicid|asesinat|sicariat|crimen|violencia", 0, 50, "Homicidios intencionales / poblacion * 100000", "anual", "Supuesto analitico interno",
  "seg_homicidios_mujeres_wb", "VC.IHR.PSRC.FE.P5", "Homicidios intencionales mujeres (por 100 mil mujeres)", "seguridad", 3.0, "por_100k", "down", "feminicid|violencia contra la mujer|homicidios mujeres", 0, 20, "Homicidios intencionales mujeres / poblacion mujeres * 100000", "anual", "Supuesto analitico interno",
  "seg_homicidios_hombres_wb", "VC.IHR.PSRC.MA.P5", "Homicidios intencionales hombres (por 100 mil hombres)", "seguridad", 9.0, "por_100k", "down", "homicidios hombres|violencia criminal|pandillas", 0, 80, "Homicidios intencionales hombres / poblacion hombres * 100000", "anual", "Supuesto analitico interno",
  "soc_pobreza_nacional_wb", "SI.POV.NAHC", "Pobreza monetaria nacional (% de poblacion)", "social", 20.0, "porcentaje", "down", "pobreza|ingresos|vulnerabilidad|exclusion", 0, 100, "Poblacion bajo linea nacional de pobreza / poblacion total * 100", "anual", "Supuesto analitico interno",
  "sal_mortalidad_materna_wb", "SH.STA.MMRT", "Mortalidad materna (por 100 mil nacidos vivos)", "salud", 40.0, "por_100k_nv", "down", "mortalidad materna|salud materna|embarazo|parto", 0, 500, "Muertes maternas / nacidos vivos * 100000", "anual", "Supuesto analitico interno",
  "sal_mortalidad_menor5_wb", "SH.DYN.MORT", "Mortalidad menores de 5 anos (por 1,000 nacidos vivos)", "salud", 12.0, "por_1000_nv", "down", "mortalidad infantil|primera infancia|ninos menores de 5", 0, 200, "Muertes menores de 5 anos / nacidos vivos * 1000", "anual", "Supuesto analitico interno",
  "sal_camas_hospitalarias_wb", "SH.MED.BEDS.ZS", "Camas hospitalarias (por 1,000 personas)", "salud", 2.2, "por_1000", "up", "hospital|camas hospitalarias|infraestructura de salud", 0, 20, "Numero de camas hospitalarias / poblacion * 1000", "anual", "Supuesto analitico interno",
  "edu_gasto_publico_pib_wb", "SE.XPD.TOTL.GD.ZS", "Gasto publico en educacion (% del PBI)", "educacion", 5.0, "porcentaje_pib", "up", "educacion|gasto educativo|presupuesto educativo|escuela", 0, 12, "Gasto publico total en educacion / PBI * 100", "anual", "Supuesto analitico interno"
)

wb_data <- map_dfr(wb_spec$wb_indicator_code, fetch_wb_latest)

wb_main <- wb_spec |>
  left_join(wb_data, by = "wb_indicator_code") |>
  mutate(
    source_institution = "World Bank (WDI)",
    source_series = coalesce(source_series, indicator_name),
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value),
    target_2031 = as.numeric(target_2031),
    better_direction = str_to_lower(better_direction),
    plausible_min = as.numeric(plausible_min),
    plausible_max = as.numeric(plausible_max)
  ) |>
  select(all_of(required_cols))

wb_sources <- wb_spec |>
  left_join(wb_data, by = "wb_indicator_code") |>
  mutate(
    source_institution = "World Bank (WDI)",
    source_series = coalesce(source_series, indicator_name),
    series_code = wb_indicator_code,
    metadata_source = "World Development Indicators",
    metadata_group = "World Bank API",
    metadata_frequency = frequency,
    metadata_updated = as.character(Sys.Date()),
    source_kind = "official_world_bank_api",
    retrieval_date = as.character(Sys.Date())
  ) |>
  transmute(
    indicator_id, indicator_name, axis,
    source_institution, source_series,
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value),
    target_2031 = as.numeric(target_2031),
    unit, better_direction = str_to_lower(better_direction),
    keyword_regex, plausible_min = as.numeric(plausible_min), plausible_max = as.numeric(plausible_max),
    kpi_formula, frequency,
    series_code,
    baseline_period,
    source_url_api,
    source_url_metadata,
    metadata_source,
    metadata_group,
    metadata_frequency,
    metadata_updated,
    target_note,
    source_kind,
    retrieval_date
  )

# Append and keep latest row per indicator_id.
main_out <- bind_rows(base_main, wb_main) |>
  group_by(indicator_id) |>
  slice_tail(n = 1) |>
  ungroup()

sources_out <- bind_rows(base_src, wb_sources) |>
  group_by(indicator_id) |>
  slice_tail(n = 1) |>
  ungroup()

write_csv(main_out, main_path)
write_csv(sources_out, sources_path)

message("Extension WB aplicada a linea base:")
message(sprintf(" - Indicadores baseline totales: %d", nrow(main_out)))
message(sprintf(" - Indicadores WB agregados: %d", nrow(wb_main)))

missing_vals <- wb_main |>
  filter(is.na(baseline_value) | is.na(baseline_year))
if (nrow(missing_vals) > 0) {
  warning(sprintf("Hay %d indicadores WB sin valor. Revisar API/series code.", nrow(missing_vals)))
}
