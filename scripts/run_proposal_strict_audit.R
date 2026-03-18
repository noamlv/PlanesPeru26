#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(tibble)
})

project_root <- normalizePath('.', winslash = '/', mustWork = TRUE)
outputs_dir <- file.path(project_root, 'outputs')

propuestas_path <- file.path(outputs_dir, 'propuestas.parquet')
if (!file.exists(propuestas_path)) stop('Falta outputs/propuestas.parquet', call. = FALSE)

propuestas <- read_parquet(propuestas_path)

# Regla estricta: mantener enunciados con longitud mínima y al menos una señal operativa dura.
# Senales duras: tipo de intervencion explicito, meta cuantitativa o horizonte temporal.
# La regla se eligio por su mejor balance conceptual y por subir precision desde 0.47 a ~0.70
# en la muestra anotada manualmente, sin vaciar demasiado el universo.
propuestas_strict <- propuestas |>
  mutate(
    strict_proposal =
      tokens_n >= 10 &
      (
        instrument_type != 'unspecified' |
          has_quant_target |
          has_time_horizon
      )
  )

strict_keep <- propuestas_strict |>
  filter(strict_proposal) |>
  select(-strict_proposal)

summary_tbl <- tibble(
  metric = c(
    'enunciados_detectados',
    'propuestas_estrictas',
    'pct_estrictas_sobre_detectadas'
  ),
  value = c(
    nrow(propuestas),
    nrow(strict_keep),
    round(100 * nrow(strict_keep) / nrow(propuestas), 1)
  )
)

party_compare <- bind_rows(
  propuestas |>
    count(party, name = 'n') |>
    mutate(universe = 'detectadas'),
  strict_keep |>
    count(party, name = 'n') |>
    mutate(universe = 'estrictas')
) |>
  tidyr::pivot_wider(names_from = universe, values_from = n, values_fill = 0) |>
  mutate(pct_strict = round(100 * estrictas / detectadas, 1)) |>
  arrange(desc(estrictas), desc(detectadas))

write_parquet(strict_keep, file.path(outputs_dir, 'propuestas_strict.parquet'))
write_csv(strict_keep, file.path(outputs_dir, 'propuestas_strict.csv'))
write_csv(summary_tbl, file.path(outputs_dir, 'proposal_count_comparison.csv'))
write_csv(party_compare, file.path(outputs_dir, 'proposal_count_comparison_by_party.csv'))

cat('Enunciados detectados:', nrow(propuestas), '\n')
cat('Propuestas estrictas:', nrow(strict_keep), '\n')
cat('Porcentaje estricto:', round(100 * nrow(strict_keep) / nrow(propuestas), 1), '%\n')
