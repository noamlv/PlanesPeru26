#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)
options(encoding = "UTF-8")

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
outputs_dir <- file.path(project_root, "outputs")
annotation_dir <- file.path(outputs_dir, "annotation")
data_dir <- file.path(project_root, "data", "plans_txt")

if (!dir.exists(annotation_dir)) dir.create(annotation_dir, recursive = TRUE)

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

pkgs <- c(
  "fs", "stringr", "stringi", "dplyr", "tidyr", "purrr", "readr", "tibble",
  "arrow", "tokenizers", "stopwords", "quanteda", "glmnet", "Matrix"
)
install_if_missing(pkgs)

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
  library(quanteda)
  library(glmnet)
  library(Matrix)
})

set.seed(20260301)

propuestas_path <- file.path(outputs_dir, "propuestas.parquet")
if (!file.exists(propuestas_path)) {
  stop("No existe outputs/propuestas.parquet. Ejecuta primero scripts/run_all.R", call. = FALSE)
}

propuestas <- read_parquet(propuestas_path)

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
  str_trim(parts[, 1])
}

file_to_doc_id <- function(file_path) {
  base <- path_ext_remove(path_file(file_path))
  base_ascii <- stri_trans_general(base, "Any-Latin; Latin-ASCII")
  base_ascii <- str_to_lower(base_ascii)
  base_ascii <- str_replace_all(base_ascii, "[^a-z0-9]+", "_")
  base_ascii <- str_replace_all(base_ascii, "^_+|_+$", "")
  paste0("doc_", base_ascii)
}

# ---------------------------
# 1) Annotation sample
# ---------------------------

files <- dir_ls(data_dir, recurse = TRUE, type = "file", regexp = "(?i)\\.txt$")
files <- files[!str_detect(path_file(files), regex("resumen\\.txt$", ignore_case = TRUE))]

if (length(files) == 0) {
  stop("No hay .txt de planes completos en data/plans_txt.", call. = FALSE)
}

docs <- tibble(
  file = files,
  party = map_chr(files, file_to_party),
  doc_id = map_chr(files, file_to_doc_id),
  text = map_chr(files, ~ normalize_utf8(read_file(.x)))
)

proposal_norm <- propuestas |>
  transmute(doc_id, proposal_id, proposal_text, proposal_norm = str_to_lower(str_squish(proposal_text)))

# Positives from extractor (stratified by party)
pos_sample <- propuestas |>
  group_by(party) |>
  group_modify(~ slice_sample(.x, n = min(nrow(.x), 6))) |>
  ungroup() |>
  transmute(
    annotation_id = paste0("ann_pos_", row_number()),
    party,
    doc_id,
    proposal_id,
    section_guess,
    candidate_text = proposal_text,
    source_type = "proposal_extractor",
    is_proposal_pred = 1L,
    axis_pred = axis,
    instrument_pred = instrument_type
  )

# Negatives from raw document sentences
extract_negative_candidates <- function(doc_id, party, text, proposal_norm_vec) {
  sents <- tokenizers::tokenize_sentences(text, lowercase = FALSE, strip_punct = FALSE)[[1]]
  if (length(sents) == 0) return(tibble())

  sents <- str_squish(sents)
  sents <- sents[nchar(sents) >= 35 & nchar(sents) <= 500]
  sents <- sents[!str_detect(sents, "^[A-ZÁÉÍÓÚÑ0-9 .,-]{4,}$")]

  norm <- str_to_lower(str_squish(sents))
  keep <- !(norm %in% proposal_norm_vec)

  out <- tibble(
    party = party,
    doc_id = doc_id,
    candidate_text = sents[keep],
    candidate_norm = norm[keep]
  ) |>
    distinct(candidate_norm, .keep_all = TRUE) |>
    select(-candidate_norm)

  out
}

neg_pool <- pmap_dfr(
  list(docs$doc_id, docs$party, docs$text),
  function(doc_id, party, text) {
    prop_norm <- proposal_norm |>
      filter(doc_id == !!doc_id) |>
      pull(proposal_norm)
    extract_negative_candidates(doc_id, party, text, prop_norm)
  }
)

neg_sample <- neg_pool |>
  group_by(party) |>
  group_modify(~ slice_sample(.x, n = min(nrow(.x), 6))) |>
  ungroup() |>
  transmute(
    annotation_id = paste0("ann_neg_", row_number()),
    party,
    doc_id,
    proposal_id = NA_character_,
    section_guess = NA_character_,
    candidate_text,
    source_type = "non_proposal_sentence",
    is_proposal_pred = 0L,
    axis_pred = NA_character_,
    instrument_pred = NA_character_
  )

annotation_sample <- bind_rows(pos_sample, neg_sample) |>
  mutate(annotation_id = paste0("ann_", str_pad(row_number(), 4, pad = "0")))

action_pattern <- "\\b(crear|creara|crearan|implementar|implementara|fortalecer|fortalecera|reformar|reformara|aumentar|incrementar|reducir|eliminar|construir|promover|impulsar|garantizar|establecer|desarrollar|financiar|ampliar|modernizar|descentralizar|reorganizar|digitalizar|formalizar)\\w*\\b"
obligation_pattern <- "\\b(se propone|se plantea|se establece|debe|deber[aá]n|se ejecutar[aá]|se crear[aá]|se implementar[aá])\\b"

annotation_sample <- annotation_sample |>
  mutate(
    candidate_lower = str_to_lower(candidate_text),
    has_action = str_detect(candidate_lower, action_pattern),
    has_obligation = str_detect(candidate_lower, obligation_pattern),
    is_proposal_seed = case_when(
      is_proposal_pred == 1L ~ as.integer(has_action | has_obligation),
      TRUE ~ as.integer(has_action & has_obligation)
    ),
    is_proposal_gold = NA_integer_,
    axis_gold = NA_character_,
    instrument_gold = NA_character_,
    annotator = NA_character_,
    notes = NA_character_
  ) |>
  select(
    annotation_id, party, doc_id, proposal_id, section_guess,
    candidate_text, source_type,
    is_proposal_pred, is_proposal_seed, is_proposal_gold,
    axis_pred, axis_gold,
    instrument_pred, instrument_gold,
    annotator, notes
  )

sample_path <- file.path(annotation_dir, "annotation_sample_v1.csv")
write_csv(annotation_sample, sample_path)

gold_path <- file.path(annotation_dir, "annotation_gold_v1.csv")
if (!file.exists(gold_path)) {
  write_csv(annotation_sample, gold_path)
}

instructions_path <- file.path(annotation_dir, "annotation_instructions.md")
write_lines(c(
  "# Guía rápida de anotación",
  "",
  "Completa `outputs/annotation/annotation_gold_v1.csv` en las columnas:",
  "- is_proposal_gold: 1 si es propuesta de política concreta, 0 si no.",
  "- axis_gold: eje temático real (solo cuando is_proposal_gold = 1).",
  "- instrument_gold: instrumento real (solo cuando is_proposal_gold = 1).",
  "",
  "Taxonomía sugerida de axis:",
  "seguridad, economia, salud, educacion, energia, empleo, institucionalidad, ambiente, infraestructura, social, otros",
  "",
  "Taxonomía sugerida de instrument:",
  "law/reform, program, spending/investment, institutional change, enforcement/punitive, technology/data, unspecified",
  "",
  "Luego ejecuta nuevamente este script para recalcular métricas con oro manual."
), instructions_path)

# Use gold if available, else seed proxy
ann_gold <- read_csv(gold_path, show_col_types = FALSE)
ann_eval <- annotation_sample |>
  select(annotation_id, is_proposal_pred) |>
  left_join(
    ann_gold |>
      select(annotation_id, is_proposal_gold, is_proposal_seed, axis_gold, instrument_gold, axis_pred, instrument_pred),
    by = "annotation_id"
  )

if (sum(!is.na(ann_eval$is_proposal_gold)) >= 30) {
  truth_prop <- ann_eval$is_proposal_gold
  extraction_metric_source <- "manual_gold"
} else {
  truth_prop <- ann_eval$is_proposal_seed
  extraction_metric_source <- "seed_proxy"
}

pred_prop <- ann_eval$is_proposal_pred

safe_div <- function(a, b) ifelse(b == 0, 0, a / b)

tp <- sum(pred_prop == 1 & truth_prop == 1, na.rm = TRUE)
fp <- sum(pred_prop == 1 & truth_prop == 0, na.rm = TRUE)
fn <- sum(pred_prop == 0 & truth_prop == 1, na.rm = TRUE)
tn <- sum(pred_prop == 0 & truth_prop == 0, na.rm = TRUE)

precision <- safe_div(tp, tp + fp)
recall <- safe_div(tp, tp + fn)
f1 <- safe_div(2 * precision * recall, precision + recall)
accuracy <- safe_div(tp + tn, tp + tn + fp + fn)

extraction_metrics <- tibble(
  metric_source = extraction_metric_source,
  sample_n = sum(!is.na(truth_prop)),
  tp = tp,
  fp = fp,
  fn = fn,
  tn = tn,
  precision = precision,
  recall = recall,
  f1 = f1,
  accuracy = accuracy
)

write_csv(extraction_metrics, file.path(outputs_dir, "extraction_metrics_v1.csv"))

extraction_conf <- tibble(
  truth = c("proposal", "proposal", "non_proposal", "non_proposal"),
  pred = c("proposal", "non_proposal", "proposal", "non_proposal"),
  n = c(tp, fn, fp, tn)
)
write_csv(extraction_conf, file.path(outputs_dir, "extraction_confusion_v1.csv"))

# ---------------------------
# 2) Supervised classifiers
# ---------------------------

build_dfm <- function(texts, feat_names = NULL, trim = TRUE) {
  toks <- quanteda::tokens(
    texts,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE
  ) |>
    quanteda::tokens_tolower() |>
    quanteda::tokens_remove(stopwords("es"))

  m <- quanteda::dfm(toks)
  if (is.null(feat_names) && trim) {
    m <- quanteda::dfm_trim(m, min_termfreq = 5)
  }
  if (!is.null(feat_names)) {
    m <- quanteda::dfm_match(m, feat_names)
  }
  as(m, "dgCMatrix")
}

stratified_folds <- function(y, k = 5) {
  y <- as.factor(y)
  fold_id <- integer(length(y))
  for (cls in levels(y)) {
    idx <- which(y == cls)
    fold_id[idx] <- sample(rep(seq_len(k), length.out = length(idx)))
  }
  fold_id
}

multiclass_metrics <- function(truth, pred) {
  truth <- as.character(truth)
  pred <- as.character(pred)
  classes <- sort(unique(c(truth, pred)))
  conf <- table(factor(truth, levels = classes), factor(pred, levels = classes))

  tp <- diag(conf)
  fn <- rowSums(conf) - tp
  fp <- colSums(conf) - tp
  support <- rowSums(conf)

  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))

  class_df <- tibble(
    class = classes,
    support = as.integer(support),
    precision = as.numeric(precision),
    recall = as.numeric(recall),
    f1 = as.numeric(f1)
  )

  overall <- tibble(
    accuracy = sum(tp) / sum(conf),
    macro_f1 = mean(class_df$f1, na.rm = TRUE),
    weighted_f1 = sum(class_df$f1 * class_df$support) / sum(class_df$support),
    n = length(truth)
  )

  list(overall = overall, by_class = class_df, conf = conf)
}

train_cv_model <- function(texts, labels, k = 5, min_class_n = 15, min_rows = 100) {
  dat <- tibble(text = texts, label = labels) |>
    filter(!is.na(label), label != "")

  class_counts <- dat |>
    count(label, name = "n")
  keep_classes <- class_counts |>
    filter(n >= min_class_n) |>
    pull(label)

  dat <- dat |>
    filter(label %in% keep_classes)

  if (nrow(dat) < min_rows || n_distinct(dat$label) < 3) {
    stop("Datos insuficientes para entrenamiento supervisado estable.")
  }

  X <- build_dfm(dat$text)
  y <- as.factor(dat$label)
  folds <- stratified_folds(y, k = k)

  pred <- rep(NA_character_, length(y))

  for (f in seq_len(k)) {
    tr <- which(folds != f)
    te <- which(folds == f)
    nfolds_inner <- max(2, min(5, as.integer(min(table(y[tr])) - 1)))

    fit_cv <- cv.glmnet(
      x = X[tr, , drop = FALSE],
      y = y[tr],
      family = "multinomial",
      type.measure = "class",
      nfolds = nfolds_inner,
      standardize = TRUE,
      maxit = 1e5
    )

    pred_te <- predict(fit_cv, newx = X[te, , drop = FALSE], s = "lambda.min", type = "class")
    pred[te] <- as.character(pred_te)
  }

  met <- multiclass_metrics(truth = y, pred = pred)

  nfolds_full <- max(2, min(5, as.integer(min(table(y)) - 1)))

  fit_full <- cv.glmnet(
    x = X,
    y = y,
    family = "multinomial",
    type.measure = "class",
    nfolds = nfolds_full,
    standardize = TRUE,
    maxit = 1e5
  )

  list(
    fit = fit_full,
    feature_names = colnames(X),
    train_data = dat,
    cv_pred = pred,
    metrics = met
  )
}

predict_cv_glmnet_multinomial <- function(model_obj, new_text) {
  X_new <- build_dfm(new_text, feat_names = model_obj$feature_names, trim = FALSE)
  pred_class <- predict(model_obj$fit, newx = X_new, s = "lambda.min", type = "class")
  pred_prob <- predict(model_obj$fit, newx = X_new, s = "lambda.min", type = "response")

  pred_class <- as.character(pred_class)

  # pred_prob can be array n x class x 1
  if (length(dim(pred_prob)) == 3) {
    prob_mat <- pred_prob[, , 1, drop = FALSE]
    prob_mat <- matrix(prob_mat, nrow = nrow(X_new), dimnames = list(NULL, dimnames(pred_prob)[[2]]))
  } else {
    prob_mat <- as.matrix(pred_prob)
  }

  max_prob <- apply(prob_mat, 1, max)

  list(class = pred_class, max_prob = max_prob)
}

# Label source for supervised learning
ann_gold_prop <- ann_gold |>
  filter(!is.na(proposal_id), proposal_id != "")

axis_manual_n <- ann_gold_prop |>
  filter(!is.na(axis_gold), axis_gold != "") |>
  nrow()

instr_manual_n <- ann_gold_prop |>
  filter(!is.na(instrument_gold), instrument_gold != "") |>
  nrow()

if (axis_manual_n >= 60) {
  axis_train <- propuestas |>
    select(proposal_id, proposal_text) |>
    inner_join(
      ann_gold_prop |>
        transmute(proposal_id, axis_label = axis_gold),
      by = "proposal_id"
    )
  axis_label_source <- "manual_gold"
} else {
  axis_train <- propuestas |>
    filter(axis_confidence >= 0.30) |>
    transmute(proposal_id, proposal_text, axis_label = axis)
  axis_label_source <- "rule_silver"
}

if (instr_manual_n >= 60) {
  instr_train <- propuestas |>
    select(proposal_id, proposal_text) |>
    inner_join(
      ann_gold_prop |>
        transmute(proposal_id, instrument_label = instrument_gold),
      by = "proposal_id"
    )
  instr_label_source <- "manual_gold"
} else {
  instr_train <- propuestas |>
    transmute(proposal_id, proposal_text, instrument_label = instrument_type)
  instr_label_source <- "rule_silver"
}

axis_min_class <- if (axis_label_source == "manual_gold") 8 else 20
instr_min_class <- if (instr_label_source == "manual_gold") 8 else 15

axis_min_rows <- if (axis_label_source == "manual_gold") 50 else 100
instr_min_rows <- if (instr_label_source == "manual_gold") 50 else 100

axis_model <- train_cv_model(
  axis_train$proposal_text,
  axis_train$axis_label,
  k = 5,
  min_class_n = axis_min_class,
  min_rows = axis_min_rows
)
instr_model <- train_cv_model(
  instr_train$proposal_text,
  instr_train$instrument_label,
  k = 5,
  min_class_n = instr_min_class,
  min_rows = instr_min_rows
)

axis_overall <- axis_model$metrics$overall |>
  mutate(model = "axis", label_source = axis_label_source)
axis_class <- axis_model$metrics$by_class |>
  mutate(model = "axis", label_source = axis_label_source)

instr_overall <- instr_model$metrics$overall |>
  mutate(model = "instrument", label_source = instr_label_source)
instr_class <- instr_model$metrics$by_class |>
  mutate(model = "instrument", label_source = instr_label_source)

write_csv(axis_overall, file.path(outputs_dir, "axis_model_metrics_v1.csv"))
write_csv(axis_class, file.path(outputs_dir, "axis_class_metrics_v1.csv"))
write_csv(instr_overall, file.path(outputs_dir, "instrument_model_metrics_v1.csv"))
write_csv(instr_class, file.path(outputs_dir, "instrument_class_metrics_v1.csv"))

saveRDS(axis_model$fit, file.path(outputs_dir, "axis_model_glmnet_v1.rds"))
saveRDS(instr_model$fit, file.path(outputs_dir, "instrument_model_glmnet_v1.rds"))

# Predict all proposals with supervised models
axis_pred_all <- predict_cv_glmnet_multinomial(axis_model, propuestas$proposal_text)
instr_pred_all <- predict_cv_glmnet_multinomial(instr_model, propuestas$proposal_text)

propuestas_supervised <- propuestas |>
  mutate(
    axis_supervised = axis_pred_all$class,
    axis_supervised_prob = axis_pred_all$max_prob,
    instrument_supervised = instr_pred_all$class,
    instrument_supervised_prob = instr_pred_all$max_prob
  )

write_parquet(propuestas_supervised, file.path(outputs_dir, "propuestas_supervised.parquet"))
write_csv(propuestas_supervised, file.path(outputs_dir, "propuestas_supervised.csv"))

summary_out <- tibble(
  artifact = c(
    "annotation_sample_v1",
    "annotation_gold_v1",
    "extraction_metrics_v1",
    "axis_model_metrics_v1",
    "instrument_model_metrics_v1",
    "propuestas_supervised"
  ),
  path = c(
    file.path("outputs", "annotation", "annotation_sample_v1.csv"),
    file.path("outputs", "annotation", "annotation_gold_v1.csv"),
    file.path("outputs", "extraction_metrics_v1.csv"),
    file.path("outputs", "axis_model_metrics_v1.csv"),
    file.path("outputs", "instrument_model_metrics_v1.csv"),
    file.path("outputs", "propuestas_supervised.parquet")
  )
)
write_csv(summary_out, file.path(outputs_dir, "validation_supervised_artifacts_v1.csv"))

message("Validación + supervisado completado.")
message(sprintf("Muestra de anotación: %d filas", nrow(annotation_sample)))
message(sprintf("Métricas extracción (%s): %s", extraction_metric_source, file.path(outputs_dir, "extraction_metrics_v1.csv")))
message(sprintf("Axis model label_source: %s", axis_label_source))
message(sprintf("Instrument model label_source: %s", instr_label_source))
