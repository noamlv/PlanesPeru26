#!/usr/bin/env Rscript

required_packages <- c("pdftools", "stringr", "fs")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Faltan paquetes requeridos: %s. Instálalos con install.packages().",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(pdftools)
  library(stringr)
  library(fs)
})

args <- commandArgs(trailingOnly = TRUE)
input_dir <- if (length(args) >= 1) args[[1]] else "data/plans_pdf"
output_dir <- if (length(args) >= 2) args[[2]] else "data/plans_txt"
outputs_dir <- if (length(args) >= 3) args[[3]] else "outputs"

if (!dir_exists(input_dir)) {
  stop(sprintf("No existe la carpeta de entrada: %s", input_dir), call. = FALSE)
}

dir_create(output_dir, recurse = TRUE)
dir_create(outputs_dir, recurse = TRUE)

error_log_path <- path(outputs_dir, "pdf_to_txt_errors.txt")
conversion_log_path <- path(outputs_dir, "pdf_to_txt_conversion_log.csv")

is_page_number_line <- function(x) {
  x <- str_squish(x)
  if (x == "") return(FALSE)
  str_detect(x, regex("^(p[aá]gina\\s+)?\\d+\\s*((de|/)\\s*\\d+)?$", ignore_case = TRUE)) ||
    str_detect(x, "^[-–—]?\\s*\\d+\\s*[-–—]?$")
}

normalize_page <- function(x) {
  x <- enc2utf8(x)
  x <- str_replace_all(x, "\\r\\n?", "\\n")
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_replace_all(x, "[\\t ]+", " ")
  x <- str_replace_all(x, "[ ]*\\n[ ]*", "\\n")
  x
}

first_non_empty_line <- function(lines) {
  vals <- str_squish(lines)
  vals <- vals[vals != ""]
  if (length(vals) == 0) "" else vals[[1]]
}

last_non_empty_line <- function(lines) {
  vals <- str_squish(lines)
  vals <- vals[vals != ""]
  if (length(vals) == 0) "" else vals[[length(vals)]]
}

clean_pdf_pages <- function(raw_pages) {
  pages <- vapply(raw_pages, normalize_page, character(1))
  page_lines <- str_split(pages, "\\n", simplify = FALSE)

  n_pages <- length(page_lines)
  first_lines <- vapply(page_lines, first_non_empty_line, character(1))
  last_lines <- vapply(page_lines, last_non_empty_line, character(1))

  threshold <- if (n_pages <= 4) 2L else max(3L, floor(n_pages * 0.5))

  header_table <- table(first_lines[first_lines != ""])
  footer_table <- table(last_lines[last_lines != ""])

  header_candidates <- names(header_table[header_table >= threshold])
  footer_candidates <- names(footer_table[footer_table >= threshold])

  header_candidates <- header_candidates[!vapply(header_candidates, is_page_number_line, logical(1))]
  footer_candidates <- footer_candidates[!vapply(footer_candidates, is_page_number_line, logical(1))]

  cleaned_pages <- character(n_pages)

  for (i in seq_len(n_pages)) {
    lines <- page_lines[[i]]
    squished <- str_squish(lines)

    keep <- squished != ""
    keep <- keep & !vapply(squished, is_page_number_line, logical(1))

    if (length(header_candidates) > 0) {
      keep <- keep & !(squished %in% header_candidates)
    }
    if (length(footer_candidates) > 0) {
      keep <- keep & !(squished %in% footer_candidates)
    }

    lines <- lines[keep]
    page <- paste(lines, collapse = "\\n")

    page <- str_replace_all(page, "-\\n(?=\\p{L})", "")
    page <- str_replace_all(page, "(?<=[\\p{L}\\p{N},;:])\\n(?=[\\p{Ll}\\p{N}])", " ")
    page <- str_replace_all(page, "\\n{3,}", "\\n\\n")
    page <- str_replace_all(page, "[\\t ]+", " ")
    page <- str_replace_all(page, "[ ]*\\n[ ]*", "\\n")

    cleaned_pages[[i]] <- str_trim(page)
  }

  combined <- ""
  for (i in seq_along(cleaned_pages)) {
    page <- cleaned_pages[[i]]
    if (page == "") next

    if (combined == "") {
      combined <- page
      next
    }

    if (str_detect(combined, "-$") && str_detect(page, "^[[:lower:]áéíóúñü]")) {
      combined <- str_replace(combined, "-$", "")
      combined <- paste0(combined, page)
    } else {
      combined <- paste(combined, page, sep = "\\n\\n")
    }
  }

  combined <- str_replace_all(combined, "\\n{3,}", "\\n\\n")
  combined <- str_trim(combined)
  enc2utf8(combined)
}

pdf_files <- sort(dir_ls(input_dir, recurse = TRUE, type = "file", regexp = "(?i)\\.pdf$"))

if (length(pdf_files) == 0) {
  writeLines(character(0), error_log_path, useBytes = TRUE)
  empty_df <- data.frame(
    pdf_file = character(0),
    txt_file = character(0),
    pages = integer(0),
    chars = integer(0),
    words = integer(0),
    stringsAsFactors = FALSE
  )
  write.csv(empty_df, conversion_log_path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Procesamiento completado: 0 exitosos, 0 fallidos, 0 total.\n")
  quit(save = "no", status = 0)
}

error_lines <- character(0)
conversion_rows <- vector("list", length(pdf_files))
success_count <- 0L
fail_count <- 0L

for (idx in seq_along(pdf_files)) {
  pdf_path <- pdf_files[[idx]]
  rel_pdf <- path_rel(pdf_path, start = input_dir)
  txt_rel <- path_ext_set(rel_pdf, "txt")
  txt_path <- path(output_dir, txt_rel)

  result <- tryCatch({
    raw_pages <- pdf_text(pdf_path)
    n_pages <- length(raw_pages)

    if (n_pages == 0) {
      stop("PDF sin páginas detectables.")
    }

    if (all(str_trim(raw_pages) == "")) {
      stop("Texto no extractable (posible PDF escaneado o protegido).")
    }

    cleaned_text <- clean_pdf_pages(raw_pages)

    if (str_trim(cleaned_text) == "") {
      stop("Texto vacío tras limpieza.")
    }

    dir_create(path_dir(txt_path), recurse = TRUE)

    con <- file(txt_path, open = "wb")
    tryCatch(
      writeBin(charToRaw(enc2utf8(cleaned_text)), con),
      finally = close(con)
    )

    words <- str_count(cleaned_text, boundary("word"))

    conversion_rows[[idx]] <- data.frame(
      pdf_file = rel_pdf,
      txt_file = txt_rel,
      pages = n_pages,
      chars = nchar(cleaned_text, type = "chars", allowNA = FALSE),
      words = words,
      stringsAsFactors = FALSE
    )

    success_count <<- success_count + 1L
    NULL
  }, error = function(e) {
    fail_count <<- fail_count + 1L
    error_lines <<- c(error_lines, sprintf("%s\t%s", rel_pdf, conditionMessage(e)))
    conversion_rows[[idx]] <<- NULL
    NULL
  })
}

if (length(error_lines) > 0) {
  writeLines(enc2utf8(error_lines), error_log_path, useBytes = TRUE)
} else {
  writeLines(character(0), error_log_path, useBytes = TRUE)
}

conversion_rows <- Filter(Negate(is.null), conversion_rows)
if (length(conversion_rows) > 0) {
  conversion_df <- do.call(rbind, conversion_rows)
} else {
  conversion_df <- data.frame(
    pdf_file = character(0),
    txt_file = character(0),
    pages = integer(0),
    chars = integer(0),
    words = integer(0),
    stringsAsFactors = FALSE
  )
}

write.csv(conversion_df, conversion_log_path, row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf(
  "Procesamiento completado: %d exitosos, %d fallidos, %d total.\n",
  success_count,
  fail_count,
  length(pdf_files)
))
cat(sprintf("Log de conversión: %s\n", conversion_log_path))
cat(sprintf("Log de errores: %s\n", error_log_path))
