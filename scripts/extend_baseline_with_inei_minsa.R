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

pkgs <- c("dplyr", "readr", "tibble", "stringr")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(stringr)
})

set.seed(20260306)

required_cols <- c(
  "indicator_id", "indicator_name", "axis", "source_institution", "source_series",
  "baseline_year", "baseline_value", "target_2031", "unit", "better_direction",
  "keyword_regex", "plausible_min", "plausible_max", "kpi_formula", "frequency"
)

base_main <- read_csv(main_path, show_col_types = FALSE)
base_src <- read_csv(sources_path, show_col_types = FALSE)
base_src <- base_src |>
  mutate(
    metadata_updated = as.character(metadata_updated),
    retrieval_date = as.character(retrieval_date),
    baseline_period = as.character(baseline_period)
  )

missing_required <- setdiff(required_cols, names(base_main))
if (length(missing_required) > 0) {
  stop(sprintf("linea_base_peru_2025.csv no tiene columnas requeridas: %s", paste(missing_required, collapse = ", ")), call. = FALSE)
}

# Remove prior WB rows and any previous INEI/MINSA manual rows to make script idempotent.
is_wb <- function(df) {
  str_detect(df$indicator_id, "_wb$") | str_detect(str_to_lower(coalesce(df$source_institution, "")), "world bank")
}

is_prev_manual <- function(df) {
  str_detect(df$indicator_id, "_(inei|minsa)$")
}

base_main <- base_main |> filter(!(is_wb(base_main) | is_prev_manual(base_main)))
base_src <- base_src |> filter(!(is_wb(base_src) | is_prev_manual(base_src)))

manual_spec <- tribble(
  ~indicator_id, ~indicator_name, ~axis, ~source_institution, ~source_series, ~baseline_year, ~baseline_value, ~target_2031, ~unit, ~better_direction, ~keyword_regex, ~plausible_min, ~plausible_max, ~kpi_formula, ~frequency, ~baseline_period, ~source_url_api, ~source_url_metadata, ~metadata_source, ~metadata_group, ~metadata_frequency, ~metadata_updated, ~target_note,
  "seg_victimizacion_urbana_inei", "Poblacion urbana victima de algun hecho delictivo (%)", "seguridad", "INEI", "Victimizacion en el Peru 2024 (ENAPRES)", 2024L, 27.1, 20.0, "porcentaje", "down", "victimiz|delincu|inseguridad ciudadana|robo|extorsion", 0, 100, "Poblacion de 15+ urbana victima de delito en ultimos 12 meses / poblacion de 15+ urbana * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "INEI", "ENAPRES - Seguridad ciudadana", "anual", "2025-05-13", "Supuesto analitico interno",
  "seg_denuncia_victima_inei", "Victimas que realizaron denuncia (%)", "seguridad", "INEI", "Victimizacion en el Peru 2024 (ENAPRES)", 2024L, 16.1, 30.0, "porcentaje", "up", "denuncia|fiscalia|comisaria|policia|delito", 0, 100, "Victimas que reportan denuncia / total de victimas * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "INEI", "ENAPRES - Seguridad ciudadana", "anual", "2025-05-13", "Supuesto analitico interno",
  "seg_victima_arma_fuego_inei", "Victimas de delito con arma de fuego (%)", "seguridad", "INEI", "Victimizacion en el Peru 2024 (ENAPRES)", 2024L, 10.1, 6.0, "porcentaje", "down", "arma de fuego|violencia armada|sicariato|delito armado", 0, 100, "Victimas de delito con arma de fuego / total de victimas * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6761498-victimizacion-en-el-peru-2024", "INEI", "ENAPRES - Seguridad ciudadana", "anual", "2025-05-13", "Supuesto analitico interno",
  "soc_pobreza_monetaria_inei", "Pobreza monetaria nacional (% de poblacion)", "social", "INEI", "Pobreza monetaria 2024", 2024L, 27.6, 20.0, "porcentaje", "down", "pobreza|ingresos|vulnerabilidad|exclusion", 0, 100, "Poblacion bajo linea de pobreza monetaria / poblacion total * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/noticias/1164173-pobreza-monetaria-afecto-al-27-6-de-la-poblacion-del-pais-en-el-ano-2024", "https://www.gob.pe/institucion/inei/informes-publicaciones/6749463-cifras-de-pobreza-2024", "INEI", "ENAHO - Pobreza monetaria", "anual", "2025-05-08", "Supuesto analitico interno",
  "sal_anemia_6a35m_inei", "Anemia en ninos de 6 a 35 meses (%)", "salud", "INEI", "ENDES 2024 - Indicadores principales", 2024L, 35.3, 25.0, "porcentaje", "down", "anemia|hierro|desnutric|primera infancia", 0, 100, "Ninos 6-35 meses con anemia / total ninos 6-35 meses * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/noticias/1177022-el-67-4-de-los-ninos-menores-de-seis-meses-recibieron-lactancia-materna", "https://proyectos.inei.gob.pe/endes/2024/departamentales/map/principal.html", "INEI", "ENDES", "anual", "2025-05-28", "Supuesto analitico interno",
  "sal_vacunacion_menor12m_inei", "Menores de 12 meses con vacunas segun edad (%)", "salud", "INEI", "ENDES 2024 - Indicadores principales", 2024L, 79.2, 90.0, "porcentaje", "up", "vacuna|inmunizacion|esquema de vacunacion|menor de 12 meses", 0, 100, "Menores de 12 meses con esquema de vacunacion segun edad / total menores de 12 meses * 100", "anual", "2024", "https://www.gob.pe/institucion/inei/noticias/1177022-el-67-4-de-los-ninos-menores-de-seis-meses-recibieron-lactancia-materna", "https://proyectos.inei.gob.pe/endes/2024/departamentales/map/principal.html", "INEI", "ENDES", "anual", "2025-05-28", "Supuesto analitico interno",
  "sal_muertes_maternas_minsa", "Muertes maternas (casos)", "salud", "MINSA", "Reporte de muertes maternas 2024", 2024L, 244.0, 180.0, "casos", "down", "mortalidad materna|salud materna|embarazo|parto", 0, 2000, "Conteo anual de casos reportados de muerte materna", "anual", "2024", "https://www.gob.pe/institucion/minsa/noticias/1141825-minsa-celebra-del-dia-mundial-de-la-salud-2025-con-enfasis-en-la-disminucion-de-la-muerte-materna", "https://www.gob.pe/institucion/minsa/noticias/1141825-minsa-celebra-del-dia-mundial-de-la-salud-2025-con-enfasis-en-la-disminucion-de-la-muerte-materna", "MINSA", "CDC Peru - Mortalidad materna", "anual", "2025-04-07", "Supuesto analitico interno",
  "edu_asistencia_secundaria_inei", "Asistencia escolar secundaria 12-16 anos (%)", "educacion", "INEI", "ENAHO 2024 (II trimestre)", 2024L, 91.5, 95.0, "porcentaje", "up", "asistencia escolar|secundaria|desercion|educacion", 0, 100, "Poblacion 12-16 que asiste a secundaria / poblacion 12-16 * 100", "trimestral", "2024-II", "https://www.gob.pe/institucion/inei/noticias/1020110-se-incremento-asistencia-escolar-de-educacion-primaria-y-secundaria", "https://www.gob.pe/institucion/inei/noticias/1020110-se-incremento-asistencia-escolar-de-educacion-primaria-y-secundaria", "INEI", "ENAHO - Estado de la Ninez y Adolescencia", "trimestral", "2024-09-10", "Supuesto analitico interno"
)

manual_main <- manual_spec |>
  transmute(
    indicator_id, indicator_name, axis,
    source_institution, source_series,
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value),
    target_2031 = as.numeric(target_2031),
    unit, better_direction = str_to_lower(better_direction),
    keyword_regex,
    plausible_min = as.numeric(plausible_min),
    plausible_max = as.numeric(plausible_max),
    kpi_formula, frequency
  )

manual_sources <- manual_spec |>
  transmute(
    indicator_id, indicator_name, axis,
    source_institution, source_series,
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value),
    target_2031 = as.numeric(target_2031),
    unit, better_direction = str_to_lower(better_direction),
    keyword_regex,
    plausible_min = as.numeric(plausible_min),
    plausible_max = as.numeric(plausible_max),
    kpi_formula, frequency,
    series_code = NA_character_,
    baseline_period,
    source_url_api,
    source_url_metadata,
    metadata_source,
    metadata_group,
    metadata_frequency,
    metadata_updated,
    target_note,
    source_kind = "official_inei_minsa_publication",
    retrieval_date = as.character(Sys.Date())
  )

main_out <- bind_rows(base_main, manual_main) |>
  group_by(indicator_id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(axis, indicator_id)

sources_out <- bind_rows(base_src, manual_sources) |>
  group_by(indicator_id) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(axis, indicator_id)

write_csv(main_out, main_path)
write_csv(sources_out, sources_path)

message("Extension INEI/MINSA aplicada a linea base:")
message(sprintf(" - Indicadores baseline totales: %d", nrow(main_out)))
message(sprintf(" - Indicadores INEI/MINSA agregados: %d", nrow(manual_main)))
message(sprintf(" - Indicadores WB presentes tras limpieza: %d", sum(str_detect(main_out$indicator_id, "_wb$") | str_detect(str_to_lower(main_out$source_institution), "world bank"))))
