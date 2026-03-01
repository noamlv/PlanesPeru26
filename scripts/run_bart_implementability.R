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

pkgs <- c("arrow", "dplyr", "tidyr", "purrr", "stringr", "readr", "tibble", "dbarts")
install_if_missing(pkgs)

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(dbarts)
})

set.seed(20260304)

propuestas_path <- file.path(outputs_dir, "propuestas_supervised.parquet")
fiscal_path <- file.path(outputs_dir, "fiscal_viability.parquet")
benchmark_path <- file.path(outputs_dir, "benchmark_gap_proposal.parquet")
kpi_path <- file.path(outputs_dir, "kpi_tracker_proposal.parquet")

required_paths <- c(propuestas_path, fiscal_path, benchmark_path, kpi_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop(sprintf("Faltan archivos requeridos para BART: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

propuestas <- read_parquet(propuestas_path)
fiscal <- read_parquet(fiscal_path)
benchmark <- read_parquet(benchmark_path)
kpi <- read_parquet(kpi_path)

base_df <- propuestas |>
  select(
    party, doc_id, proposal_id,
    axis_supervised, instrument_supervised,
    tokens_n,
    has_quant_target, has_time_horizon,
    mentions_cost, mentions_funding_source,
    population_target, territory_target,
    evidence_citation_guess,
    vague_flag,
    axis_supervised_prob, instrument_supervised_prob,
    concreteness_score,
    source_snippet
  ) |>
  left_join(
    fiscal |>
      select(
        party, doc_id, proposal_id,
        fiscal_viability_score, fiscal_impact_band,
        has_macro_anchor, high_impact_without_funding,
        estimated_amount_pen
      ),
    by = c("party", "doc_id", "proposal_id")
  ) |>
  left_join(
    benchmark |>
      select(
        party, doc_id, proposal_id,
        benchmark_alignment_score,
        map_quality, keyword_hits,
        direction_consistent, benchmark_status
      ),
    by = c("party", "doc_id", "proposal_id")
  ) |>
  left_join(
    kpi |>
      select(
        party, doc_id, proposal_id,
        verifiability_score,
        is_kpi_verifiable,
        data_frequency
      ),
    by = c("party", "doc_id", "proposal_id")
  )

model_df <- base_df |>
  mutate(
    has_population_target = !is.na(population_target) & str_trim(population_target) != "",
    has_territory_target = !is.na(territory_target) & str_trim(territory_target) != "",
    has_evidence = !is.na(evidence_citation_guess) & str_trim(evidence_citation_guess) != "",
    amount_log1p = log1p(coalesce(estimated_amount_pen, 0)),
    keyword_hits = coalesce(keyword_hits, 0),
    axis_supervised = if_else(is.na(axis_supervised) | axis_supervised == "", "otros", axis_supervised),
    instrument_supervised = if_else(is.na(instrument_supervised) | instrument_supervised == "", "unspecified", instrument_supervised),
    fiscal_impact_band = if_else(is.na(fiscal_impact_band) | fiscal_impact_band == "", "unknown", fiscal_impact_band),
    map_quality = if_else(is.na(map_quality) | map_quality == "", "baja", map_quality),
    benchmark_status = if_else(is.na(benchmark_status) | benchmark_status == "", "indeterminado", benchmark_status),
    direction_consistent_cat = case_when(
      is.na(direction_consistent) ~ "unknown",
      direction_consistent ~ "yes",
      TRUE ~ "no"
    ),
    data_frequency = if_else(is.na(data_frequency) | data_frequency == "", "desconocida", data_frequency),
    implementation_readiness_score = round(
      0.35 * coalesce(concreteness_score, 0) +
        0.30 * coalesce(fiscal_viability_score, 0) +
        0.20 * coalesce(benchmark_alignment_score, 0) +
        0.15 * coalesce(verifiability_score, 0),
      1
    ),
    implementation_readiness_score = pmin(100, pmax(0, implementation_readiness_score))
  )

feature_df <- model_df |>
  transmute(
    tokens_n = as.numeric(coalesce(tokens_n, 0)),
    axis_supervised_prob = as.numeric(coalesce(axis_supervised_prob, 0)),
    instrument_supervised_prob = as.numeric(coalesce(instrument_supervised_prob, 0)),
    has_quant_target = as.integer(coalesce(has_quant_target, FALSE)),
    has_time_horizon = as.integer(coalesce(has_time_horizon, FALSE)),
    mentions_cost = as.integer(coalesce(mentions_cost, FALSE)),
    mentions_funding_source = as.integer(coalesce(mentions_funding_source, FALSE)),
    vague_flag = as.integer(coalesce(vague_flag, FALSE)),
    has_population_target = as.integer(coalesce(has_population_target, FALSE)),
    has_territory_target = as.integer(coalesce(has_territory_target, FALSE)),
    has_evidence = as.integer(coalesce(has_evidence, FALSE)),
    fiscal_viability_score = as.numeric(coalesce(fiscal_viability_score, 0)),
    has_macro_anchor = as.integer(coalesce(has_macro_anchor, FALSE)),
    high_impact_without_funding = as.integer(coalesce(high_impact_without_funding, FALSE)),
    amount_log1p = as.numeric(coalesce(amount_log1p, 0)),
    keyword_hits = as.numeric(coalesce(keyword_hits, 0)),
    is_kpi_verifiable = as.integer(coalesce(is_kpi_verifiable, FALSE)),
    axis_supervised = factor(axis_supervised),
    instrument_supervised = factor(instrument_supervised),
    fiscal_impact_band = factor(fiscal_impact_band),
    map_quality = factor(map_quality),
    direction_consistent_cat = factor(direction_consistent_cat),
    benchmark_status = factor(benchmark_status),
    data_frequency = factor(data_frequency)
  )

# Remove degenerate categorical predictors (single observed level) to keep model.matrix stable.
feature_df <- feature_df |>
  mutate(across(where(is.factor), droplevels))

single_level_factors <- names(feature_df)[vapply(feature_df, function(col) is.factor(col) && nlevels(col) < 2, logical(1))]
if (length(single_level_factors) > 0) {
  feature_df <- feature_df |>
    select(-all_of(single_level_factors))
}

y <- model_df$implementation_readiness_score

x <- model.matrix(~ . - 1, data = feature_df)

n <- nrow(x)
if (n < 500) stop("Muestra insuficiente para BART estable.", call. = FALSE)

train_n <- floor(0.80 * n)
train_idx <- sample(seq_len(n), size = train_n)
test_idx <- setdiff(seq_len(n), train_idx)

x_train <- x[train_idx, , drop = FALSE]
y_train <- y[train_idx]
x_test <- x[test_idx, , drop = FALSE]
y_test <- y[test_idx]

bart_fit <- dbarts::bart(
  x.train = x_train,
  y.train = y_train,
  x.test = x_test,
  keeptrees = TRUE,
  verbose = FALSE,
  ntree = 140,
  nskip = 500,
  ndpost = 700
)

pred_test <- as.numeric(bart_fit$yhat.test.mean)
rmse <- sqrt(mean((y_test - pred_test)^2))
mae <- mean(abs(y_test - pred_test))
r2 <- 1 - sum((y_test - pred_test)^2) / sum((y_test - mean(y_test))^2)

readiness_threshold <- as.numeric(quantile(y_train, probs = 0.75, na.rm = TRUE))
actual_high <- y_test >= readiness_threshold
pred_high <- pred_test >= readiness_threshold

precision <- if (sum(pred_high) == 0) NA_real_ else sum(pred_high & actual_high) / sum(pred_high)
recall <- if (sum(actual_high) == 0) NA_real_ else sum(pred_high & actual_high) / sum(actual_high)
accuracy <- mean(pred_high == actual_high)
f1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_ else 2 * precision * recall / (precision + recall)

metrics <- tibble(
  metric = c(
    "n_total", "n_train", "n_test",
    "readiness_threshold_q75",
    "rmse_test", "mae_test", "r2_test",
    "accuracy_high_readiness", "precision_high_readiness",
    "recall_high_readiness", "f1_high_readiness"
  ),
  value = c(
    n, length(train_idx), length(test_idx),
    readiness_threshold,
    rmse, mae, r2,
    accuracy, precision,
    recall, f1
  )
)

write_csv(metrics, file.path(outputs_dir, "bart_metrics.csv"))

pred_all_post <- predict(bart_fit, newdata = x)
pred_all <- colMeans(pred_all_post)
pred_sd <- apply(pred_all_post, 2, sd)

score_to_tier <- function(s) {
  case_when(
    s >= 70 ~ "alta",
    s >= 45 ~ "media",
    TRUE ~ "baja"
  )
}

predictions <- model_df |>
  transmute(
    party,
    doc_id,
    proposal_id,
    implementation_readiness_score,
    pred_readiness_score = pred_all,
    pred_uncertainty_sd = pred_sd,
    residual = implementation_readiness_score - pred_readiness_score,
    actual_tier = score_to_tier(implementation_readiness_score),
    pred_tier = score_to_tier(pred_readiness_score),
    source_snippet
  )

write_parquet(predictions, file.path(outputs_dir, "bart_predictions.parquet"))
write_csv(predictions, file.path(outputs_dir, "bart_predictions.csv"))

party_diagnostics <- predictions |>
  group_by(party) |>
  summarise(
    proposals_n = n(),
    actual_mean = mean(implementation_readiness_score, na.rm = TRUE),
    pred_mean = mean(pred_readiness_score, na.rm = TRUE),
    residual_mean = mean(residual, na.rm = TRUE),
    residual_abs_mean = mean(abs(residual), na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(actual_mean))

write_parquet(party_diagnostics, file.path(outputs_dir, "bart_party_diagnostics.parquet"))
write_csv(party_diagnostics, file.path(outputs_dir, "bart_party_diagnostics.csv"))

var_imp <- tibble(
  feature = colnames(x),
  split_count_mean = colMeans(bart_fit$varcount)
) |>
  mutate(
    importance_pct = 100 * split_count_mean / sum(split_count_mean)
  ) |>
  arrange(desc(importance_pct))

write_parquet(var_imp, file.path(outputs_dir, "bart_variable_importance.parquet"))
write_csv(var_imp, file.path(outputs_dir, "bart_variable_importance.csv"))

# Partial dependence on top features
x_ref <- x[sample(seq_len(n), size = min(1200, n)), , drop = FALSE]
top_features <- head(var_imp$feature, 8)

pdp_list <- map(top_features, function(ft) {
  vec <- x_ref[, ft]
  uv <- sort(unique(vec))

  if (length(uv) <= 2) {
    grid <- uv
  } else {
    grid <- as.numeric(unique(round(quantile(vec, probs = seq(0.1, 0.9, by = 0.2), na.rm = TRUE), 6)))
  }

  if (length(grid) == 0) grid <- mean(vec, na.rm = TRUE)

  map_dfr(grid, function(gv) {
    x_tmp <- x_ref
    x_tmp[, ft] <- gv
    pmat <- predict(bart_fit, newdata = x_tmp)
    draw_means <- rowMeans(pmat)

    tibble(
      feature = ft,
      feature_value = as.numeric(gv),
      pred_mean = mean(draw_means),
      pred_lwr = as.numeric(quantile(draw_means, 0.10)),
      pred_upr = as.numeric(quantile(draw_means, 0.90)),
      n_ref = nrow(x_tmp)
    )
  })
})

pdp <- bind_rows(pdp_list) |>
  arrange(feature, feature_value)

write_parquet(pdp, file.path(outputs_dir, "bart_partial_dependence.parquet"))
write_csv(pdp, file.path(outputs_dir, "bart_partial_dependence.csv"))

saveRDS(bart_fit, file.path(outputs_dir, "bart_implementability_model.rds"))

summary_tbl <- tibble(
  metric = c(
    "readiness_score_mean",
    "readiness_score_sd",
    "readiness_threshold_q75",
    "top_feature_1",
    "top_feature_2",
    "top_feature_3"
  ),
  value = c(
    round(mean(y, na.rm = TRUE), 2),
    round(sd(y, na.rm = TRUE), 2),
    round(readiness_threshold, 2),
    ifelse(nrow(var_imp) >= 1, var_imp$feature[1], NA_character_),
    ifelse(nrow(var_imp) >= 2, var_imp$feature[2], NA_character_),
    ifelse(nrow(var_imp) >= 3, var_imp$feature[3], NA_character_)
  )
)

write_csv(summary_tbl, file.path(outputs_dir, "bart_summary.csv"))

message("Bloque BART implementabilidad completado:")
message(sprintf(" - Observaciones: %d", n))
message(sprintf(" - RMSE test: %.3f", rmse))
message(sprintf(" - R2 test: %.3f", r2))
message(sprintf(" - Umbral alto (q75): %.2f", readiness_threshold))
message(sprintf(" - Top feature 1: %s", var_imp$feature[1]))
