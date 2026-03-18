#!/usr/bin/env Rscript

required_pkgs <- c("shiny", "arrow", "dplyr", "stringr", "stringi", "glue", "jsonlite")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(sprintf("Faltan paquetes para abrir el explorador local: %s", paste(missing_pkgs, collapse = ", ")), call. = FALSE)
}

library(shiny)
library(arrow)
library(dplyr)
library(stringr)
library(stringi)
library(glue)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

find_root <- function() {
  file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  candidates <- c(getwd())
  if (length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    candidates <- c(dirname(normalizePath(script_path, mustWork = FALSE)), candidates)
  }
  candidates <- unique(normalizePath(candidates, mustWork = FALSE))

  for (cand in candidates) {
    if (file.exists(file.path(cand, "outputs", "propuestas_supervised.parquet"))) {
      return(cand)
    }
    if (file.exists(file.path(cand, "..", "outputs", "propuestas_supervised.parquet"))) {
      return(normalizePath(file.path(cand, ".."), mustWork = TRUE))
    }
  }

  stop("No pude ubicar la raíz del proyecto. Ejecuta el script desde Planes_gobierno/ o scripts/.", call. = FALSE)
}

root_dir <- find_root()
propuestas_path <- file.path(root_dir, "outputs", "propuestas_supervised.parquet")

normalize_text <- function(x) {
  x <- coalesce(as.character(x), "")
  x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

party_pretty <- function(x) {
  x_norm <- normalize_text(x)
  pretty_map <- c(
    "ahora nacion" = "Ahora Nación",
    "alianza electoral venceremos" = "Alianza Electoral Venceremos",
    "alianza para el progreso" = "Alianza para el Progreso",
    "avanza pais" = "Avanza País",
    "fe en el peru" = "Fe en el Perú",
    "fuerza popular" = "Fuerza Popular",
    "fuerza y libertad" = "Fuerza y Libertad",
    "juntos por el peru" = "Juntos por el Perú",
    "libertad popular" = "Libertad Popular",
    "partido aprista peruano" = "Partido Aprista Peruano",
    "partido civico obras" = "Partido Cívico Obras",
    "pte peru" = "PTE Perú",
    "partido del buen gobierno" = "Partido del Buen Gobierno",
    "partido democrata unido peru" = "Partido Demócrata Unido Perú",
    "partido democrata verde" = "Partido Demócrata Verde",
    "partido democratico federal" = "Partido Democrático Federal",
    "somos peru" = "Somos Perú",
    "frente de la esperanza" = "Frente de la Esperanza",
    "partido morado" = "Partido Morado",
    "pais para todos" = "País para Todos",
    "partido patriotico del peru" = "Partido Patriótico del Perú",
    "cooperacion popular" = "Cooperación Popular",
    "integridad democratica" = "Integridad Democrática",
    "peru libre" = "Perú Libre",
    "peru accion" = "Perú Acción",
    "peru primero" = "Perú Primero",
    "prin" = "PRIN",
    "si creo" = "Sí Creo",
    "peru moderno" = "Perú Moderno",
    "podemos peru" = "Podemos Perú",
    "progresemos" = "Progresemos",
    "renovacion popular" = "Renovación Popular",
    "salvemos al peru" = "Salvemos al Perú",
    "un camino diferente" = "Un Camino Diferente",
    "unidad nacional" = "Unidad Nacional",
    "fia del peru" = "FIA del Perú",
    "primero la gente" = "Primero la Gente",
    "buen gobierno" = "Partido del Buen Gobierno",
    "obras" = "Partido Cívico Obras",
    "partido aprista" = "Partido Aprista Peruano",
    "partido de los trabajadores y emprendedores" = "Partido de los Trabajadores y Emprendedores",
    "partido unido peru" = "Partido Demócrata Unido Perú",
    "democrata verde" = "Partido Demócrata Verde",
    "venceremos" = "Alianza Electoral Venceremos"
  )
  out <- unname(pretty_map[x_norm])
  ifelse(is.na(out), x, out)
}

axis_pretty <- function(x) {
  x_norm <- normalize_text(x)
  out <- dplyr::case_when(
    x_norm == "ambiente" ~ "Ambiente",
    x_norm == "economia" ~ "Economía",
    x_norm == "educacion" ~ "Educación",
    x_norm == "empleo" ~ "Empleo",
    x_norm == "energia" ~ "Energía",
    x_norm == "infraestructura" ~ "Infraestructura",
    x_norm == "institucionalidad" ~ "Institucionalidad",
    x_norm == "otros" ~ "Otros",
    x_norm == "salud" ~ "Salud",
    x_norm == "seguridad" ~ "Seguridad",
    x_norm == "social" ~ "Social",
    TRUE ~ stringr::str_to_title(x)
  )
  ifelse(is.na(out), "Otros", out)
}

instrument_pretty <- function(x) {
  x_norm <- normalize_text(x)
  dplyr::case_when(
    x_norm == "law reform" ~ "Ley o reforma",
    x_norm == "program" ~ "Programa",
    x_norm == "spending investment" ~ "Gasto o inversión",
    x_norm == "institutional change" ~ "Cambio institucional",
    x_norm == "enforcement punitive" ~ "Fiscalización o sanción",
    x_norm == "technology data" ~ "Tecnología o datos",
    x_norm == "unspecified" ~ "Sin tipo explícito",
    TRUE ~ stringr::str_to_sentence(str_replace_all(x_norm, " ", " "))
  )
}

propuestas <- read_parquet(propuestas_path) |>
  transmute(
    party = party_pretty(as.character(party)),
    doc_id = as.character(doc_id),
    proposal_id = as.character(proposal_id),
    axis = axis_pretty(coalesce(axis, axis_supervised, "otros")),
    proposal_text = str_squish(as.character(proposal_text)),
    source_snippet = str_squish(as.character(source_snippet)),
    concreteness_score = as.numeric(coalesce(concreteness_score, 0)),
    has_quant_target = as.logical(coalesce(has_quant_target, FALSE)),
    has_time_horizon = as.logical(coalesce(has_time_horizon, FALSE)),
    instrument_type = instrument_pretty(coalesce(instrument_type, instrument_supervised, "unspecified"))
  ) |>
  mutate(
    corpus = normalize_text(paste(party, axis, proposal_text, source_snippet, instrument_type)),
    party_key = normalize_text(party),
    axis_key = normalize_text(axis)
  )

all_parties <- sort(unique(propuestas$party))
all_axes <- sort(unique(propuestas$axis))

score_query <- function(df, query) {
  query_norm <- normalize_text(query)
  tokens <- unique(unlist(str_split(query_norm, " ")))
  tokens <- tokens[nchar(tokens) >= 2]

  if (!nzchar(query_norm)) {
    return(df |>
      mutate(search_score = (concreteness_score / 20) + if_else(has_quant_target, 0.5, 0) + if_else(has_time_horizon, 0.5, 0)))
  }

  score_one <- function(corpus, concreteness, has_quant, has_time) {
    score <- 0
    if (str_detect(corpus, fixed(query_norm))) score <- score + 8
    if (length(tokens) > 0) {
      score <- score + sum(vapply(tokens, function(tok) str_detect(corpus, fixed(tok)), logical(1))) * 2.2
    }
    score <- score + (concreteness / 30)
    if (isTRUE(has_quant)) score <- score + 0.8
    if (isTRUE(has_time)) score <- score + 0.8
    score
  }

  df |>
    rowwise() |>
    mutate(search_score = score_one(corpus, concreteness_score, has_quant_target, has_time_horizon)) |>
    ungroup()
}

retrieve_proposals <- function(query, party = NULL, axis = NULL, n = 8) {
  df <- propuestas
  if (!is.null(party) && nzchar(party)) df <- filter(df, party == party)
  if (!is.null(axis) && nzchar(axis)) df <- filter(df, axis == axis)

  df <- score_query(df, query) |>
    arrange(desc(search_score), desc(concreteness_score), desc(has_quant_target), desc(has_time_horizon))

  if (nzchar(normalize_text(query))) {
    df <- filter(df, search_score > 0)
  }

  slice_head(df, n = n)
}

provider_info <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    return(list(enabled = FALSE, provider = NULL, model = NULL))
  }
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    return(list(enabled = TRUE, provider = "openai", model = Sys.getenv("PLANOMETRO_ELLMER_MODEL", unset = "gpt-4.1-mini")))
  }
  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    return(list(enabled = TRUE, provider = "anthropic", model = Sys.getenv("PLANOMETRO_ELLMER_MODEL", unset = "claude-3-5-sonnet-latest")))
  }
  list(enabled = FALSE, provider = NULL, model = NULL)
}

build_context <- function(rows) {
  if (nrow(rows) == 0) return("")
  pieces <- lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]
    glue(
      "Fuente {i}\n",
      "Partido: {row$party}\n",
      "Tema: {row$axis}\n",
      "doc_id: {row$doc_id}\n",
      "proposal_id: {row$proposal_id}\n",
      "Tipo de intervención: {row$instrument_type}\n",
      "Texto: {row$proposal_text}\n",
      "Snippet: {row$source_snippet}\n"
    )
  })
  paste(unlist(pieces), collapse = "\n---\n")
}

ask_llm <- function(query, rows) {
  provider <- provider_info()
  if (!provider$enabled) {
    return(list(ok = FALSE, text = "La versión conversacional con IA no está activa en este equipo. Puedes seguir usando la búsqueda asistida de propuestas y, si luego agregas una API key, este módulo responderá con un resumen narrativo sustentado en esas mismas fuentes."))
  }

  system_prompt <- paste(
    "Eres un asistente para analizar planes de gobierno del Perú.",
    "Responde siempre en español.",
    "Usa exclusivamente la evidencia que se te entrega.",
    "No inventes propuestas, partidos, cifras ni relaciones que no estén en las fuentes.",
    "Si la evidencia no alcanza, dilo con claridad.",
    "Cuando afirmes algo, cita entre corchetes el partido, doc_id y proposal_id, por ejemplo [Partido X | doc_1 | prop_2].",
    "Entrega una respuesta breve, clara y útil para una persona no técnica."
  )

  chat <- switch(
    provider$provider,
    openai = ellmer::chat_openai(model = provider$model, system_prompt = system_prompt),
    anthropic = ellmer::chat_anthropic(model = provider$model, system_prompt = system_prompt),
    stop("Proveedor no soportado", call. = FALSE)
  )

  user_prompt <- glue(
    "Pregunta del usuario: {query}\n\n",
    "Fuentes recuperadas:\n{build_context(rows)}\n\n",
    "Instrucciones de respuesta:\n",
    "1. Responde en uno o dos párrafos.\n",
    "2. Señala similitudes o diferencias entre partidos solo si aparecen en las fuentes.\n",
    "3. Cierra con una línea que diga 'Fuentes clave:' y menciona dos o tres citas breves entre corchetes."
  )

  out <- tryCatch(chat$chat(user_prompt, echo = FALSE), error = function(e) e)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, text = paste("No pude generar la respuesta con IA en este momento:", conditionMessage(out))))
  }

  list(ok = TRUE, text = paste(as.character(out), collapse = "\n"), provider = provider$provider, model = provider$model)
}

fallback_summary <- function(rows) {
  if (nrow(rows) == 0) {
    return("No encontré propuestas relevantes con ese criterio. Prueba otra palabra, un partido o un tema más específico.")
  }
  top_parties <- rows |>
    count(party, sort = TRUE) |>
    slice_head(n = 3) |>
    pull(party)
  top_axes <- rows |>
    count(axis, sort = TRUE) |>
    slice_head(n = 2) |>
    pull(axis)

  glue(
    "Encontré {nrow(rows)} propuestas relevantes. Los partidos que más aparecen en esta búsqueda son {paste(top_parties, collapse = ', ')}, y los temas que más se repiten son {paste(top_axes, collapse = ' y ')}. Usa las tarjetas de abajo para leer la evidencia exacta antes de sacar una conclusión."
  )
}

example_prompts <- c(
  "¿Qué proponen los partidos sobre seguridad ciudadana?",
  "¿Qué diferencias hay entre Fuerza Popular y Renovación Popular en economía?",
  "¿Qué planes mencionan canon, APP u obras por impuestos?",
  "¿Qué propuestas hablan de salud mental o atención primaria?"
)

ui <- fluidPage(
  tags$head(
    tags$title("Explorar con IA · Planómetro 2026"),
    tags$style(HTML("\n      :root { --bg: #ffffff; --text: #111111; --muted: #6b7280; --line: #e5e7eb; --accent: #111111; }\n      body { background: var(--bg); color: var(--text); font-family: \"Söhne\", \"Soehne\", Inter, -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Helvetica Neue\", Arial, sans-serif; }\n      .app-wrap { max-width: 1200px; margin: 0 auto; padding: 28px 18px 42px; }\n      .app-kicker { color: var(--muted); font-size: 14px; margin-bottom: 12px; text-align: center; }\n      .app-title { font-size: clamp(2rem, 4vw, 3.1rem); line-height: 1.02; letter-spacing: -0.02em; text-align: center; margin: 0 0 14px; font-weight: 460; }\n      .app-lead { max-width: 860px; margin: 0 auto 24px; text-align: center; font-size: 1.04rem; line-height: 1.58; }\n      .app-grid { display: grid; grid-template-columns: 340px minmax(0, 1fr); gap: 22px; align-items: start; }\n      .panel { border: 1px solid var(--line); border-radius: 18px; padding: 18px; background: #fff; }\n      .panel h3 { margin: 0 0 10px; font-size: 1rem; font-weight: 600; }\n      .field-label { display: block; margin-bottom: 6px; font-size: 0.9rem; color: var(--muted); }\n      .panel input, .panel textarea, .panel select { border-radius: 14px; border: 1px solid var(--line); box-shadow: none !important; }\n      .prompt-row { display: flex; flex-wrap: wrap; gap: 8px; margin: 10px 0 0; }\n      .prompt-chip { border: 1px solid var(--line); border-radius: 999px; background: #fff; color: var(--text); padding: 8px 12px; font-size: 0.88rem; cursor: pointer; }\n      .prompt-chip:hover { border-color: #cfd4da; background: #fafafa; }\n      .btn-row { display: flex; gap: 10px; flex-wrap: wrap; margin-top: 14px; }\n      .btn-main, .btn-light { border-radius: 999px; padding: 10px 16px; border: 1px solid var(--text); font-size: 0.92rem; font-weight: 500; cursor: pointer; }\n      .btn-main { background: var(--text); color: #fff; }\n      .btn-light { background: #fff; color: var(--text); }\n      .status-box { margin-bottom: 16px; padding: 12px 14px; border-radius: 14px; background: #f8f8f8; color: var(--muted); font-size: 0.92rem; }\n      .answer-box { border: 1px solid var(--line); border-radius: 18px; padding: 18px; background: #fff; margin-bottom: 16px; }\n      .answer-title { font-size: 0.98rem; font-weight: 600; color: #5b5b5b; margin-bottom: 10px; }\n      .answer-text { font-size: 1rem; line-height: 1.66; }\n      .source-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 14px; }\n      .source-card { border: 1px solid var(--line); border-radius: 18px; padding: 16px; background: #fff; }\n      .source-party { font-size: 1rem; font-weight: 600; margin-bottom: 8px; }\n      .chip-row { display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 10px; }\n      .chip { border-radius: 999px; background: #f3f4f6; color: #374151; padding: 4px 10px; font-size: 0.78rem; }\n      .source-text { font-size: 0.95rem; line-height: 1.58; margin-bottom: 8px; }\n      .source-details { color: var(--muted); font-size: 0.82rem; }\n      @media (max-width: 920px) { .app-grid { grid-template-columns: 1fr; } }\n    "))
  ),
  div(
    class = "app-wrap",
    div(class = "app-kicker", "Planómetro 2026 · exploración asistida"),
    h1(class = "app-title", "Explorar con IA"),
    p(class = "app-lead", "Este módulo combina búsqueda de propuestas con una capa opcional de IA. Primero localiza los fragmentos más relevantes del plan y luego, si hay credenciales activas, usa ellmer para redactar una respuesta breve en español con citas al partido, doc_id y proposal_id."),
    div(
      class = "app-grid",
      div(
        class = "panel",
        h3("Tu pregunta"),
        tags$label(class = "field-label", `for` = "query", "Escribe una pregunta, un tema o una comparación"),
        textAreaInput("query", label = NULL, rows = 4, width = "100%", placeholder = "Ejemplo: ¿Qué diferencias hay entre Fuerza Popular y Juntos por el Perú en salud?"),
        tags$label(class = "field-label", `for` = "party", "Filtrar por partido"),
        selectInput("party", label = NULL, choices = c("Todos" = "", all_parties), width = "100%"),
        tags$label(class = "field-label", `for` = "axis", "Filtrar por tema"),
        selectInput("axis", label = NULL, choices = c("Todos" = "", all_axes), width = "100%"),
        tags$label(class = "field-label", `for` = "n_results", "Cuántas propuestas recuperar"),
        sliderInput("n_results", label = NULL, min = 4, max = 12, value = 8, step = 1, width = "100%"),
        checkboxInput("use_ai", "Redactar una respuesta con IA si hay credenciales activas", value = TRUE),
        div(class = "btn-row",
            actionButton("search", "Buscar", class = "btn-main"),
            actionButton("clear", "Limpiar", class = "btn-light")
        ),
        h3(style = "margin-top:18px;", "Preguntas sugeridas"),
        div(
          class = "prompt-row",
          lapply(example_prompts, function(txt) {
            tags$button(type = "button", class = "prompt-chip", onclick = sprintf("Shiny.setInputValue('preset_prompt', %s, {priority: 'event'})", jsonlite::toJSON(txt, auto_unbox = TRUE)), txt)
          })
        )
      ),
      div(
        uiOutput("status_ui"),
        uiOutput("answer_ui"),
        div(class = "panel",
            h3("Fuentes recuperadas"),
            uiOutput("sources_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$preset_prompt, {
    updateTextAreaInput(session, "query", value = input$preset_prompt)
  })

  observeEvent(input$clear, {
    updateTextAreaInput(session, "query", value = "")
    updateSelectInput(session, "party", selected = "")
    updateSelectInput(session, "axis", selected = "")
    updateSliderInput(session, "n_results", value = 8)
    updateCheckboxInput(session, "use_ai", value = TRUE)
  })

  provider <- reactive(provider_info())

  output$status_ui <- renderUI({
    prov <- provider()
    txt <- if (prov$enabled) {
      glue("La respuesta conversacional está activa con {prov$provider} ({prov$model}). Aun así, abajo siempre verás las propuestas fuente para poder auditar la respuesta.")
    } else {
      "La versión conversacional con IA no está activa en este equipo. Puedes usar la búsqueda asistida y, cuando agregues una API key, este mismo módulo responderá con un resumen redactado sobre la evidencia recuperada."
    }
    div(class = "status-box", txt)
  })

  retrieved <- eventReactive(input$search, {
    retrieve_proposals(input$query %||% "", party = input$party %||% NULL, axis = input$axis %||% NULL, n = input$n_results %||% 8)
  }, ignoreNULL = FALSE)

  answer <- eventReactive(input$search, {
    rows <- retrieved()
    if (isTRUE(input$use_ai) && nzchar(str_squish(input$query %||% ""))) {
      ask_llm(input$query, rows)
    } else {
      list(ok = FALSE, text = fallback_summary(rows))
    }
  }, ignoreNULL = FALSE)

  output$answer_ui <- renderUI({
    ans <- answer()
    rows <- retrieved()
    title <- if (isTRUE(input$use_ai) && provider()$enabled && nzchar(str_squish(input$query %||% ""))) "Respuesta asistida" else "Lectura rápida"
    div(
      class = "answer-box",
      div(class = "answer-title", title),
      div(class = "answer-text", lapply(strsplit(ans$text, "\n\n")[[1]], function(par) tags$p(par)))
    )
  })

  output$sources_ui <- renderUI({
    rows <- retrieved()
    if (nrow(rows) == 0) {
      return(tags$p(class = "answer-text", "No encontré propuestas con ese criterio. Prueba otra palabra, otro partido o un tema más específico."))
    }
    div(
      class = "source-grid",
      lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        div(
          class = "source-card",
          div(class = "source-party", row$party),
          div(class = "chip-row",
              span(class = "chip", row$axis),
              span(class = "chip", row$instrument_type),
              span(class = "chip", sprintf("Claridad %.1f", row$concreteness_score))
          ),
          p(class = "source-text", row$proposal_text),
          p(class = "source-text", row$source_snippet),
          div(class = "source-details", glue("doc_id: {row$doc_id} · proposal_id: {row$proposal_id}"))
        )
      })
    )
  })
}

app <- shinyApp(ui, server)

if (interactive()) {
  app
} else {
  port <- as.integer(Sys.getenv("PLANOMETRO_APP_PORT", unset = "0"))
  if (!is.finite(port) || port <= 0) {
    port <- httpuv::randomPort()
  }
  shiny::runApp(
    app,
    launch.browser = TRUE,
    port = port,
    host = Sys.getenv("PLANOMETRO_APP_HOST", unset = "127.0.0.1")
  )
}
