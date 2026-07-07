# app.R
# DDC / Escape!! ranked pairings + scoring (4–10 players)
# Field-proofed: autosave to LocalStorage, autoreconnect, heartbeat keep-alive,
# round-based per-player differentials, two-court rounds collapsed by round.

library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)
library(jsonlite)
library(googlesheets4)

# ---------- Helpers ------------------------------------------------------------

# Keep player indices so we can derive byes automatically
mk_games_df <- function(p, specs) {
  rounds <- purrr::map_int(specs, ~ as.integer(.x$round))
  courts <- purrr::map_int(specs, ~ if (is.null(.x$court)) 1L else as.integer(.x$court))
  teamA_i <- lapply(specs, `[[`, "A")
  teamB_i <- lapply(specs, `[[`, "B")
  
  tibble(
    game_id = seq_along(specs),
    round   = rounds,
    court   = courts,
    A_idx   = I(teamA_i),
    B_idx   = I(teamB_i)
  ) %>%
    mutate(
      teamA = purrr::map_chr(A_idx, ~ paste(p[.x], collapse = " / ")),
      teamB = purrr::map_chr(B_idx, ~ paste(p[.x], collapse = " / "))
    ) %>%
    select(game_id, round, court, A_idx, B_idx, teamA, teamB)
}

# Derive byes per round from which players appear in games that round
compute_byes_from_games <- function(p, games_df) {
  rounds <- sort(unique(games_df$round))
  out <- purrr::map_dfr(rounds, function(r) {
    used_idx <- sort(unique(unlist(c(games_df$A_idx[games_df$round == r],
                                     games_df$B_idx[games_df$round == r]))))
    bye_idx <- setdiff(seq_along(p), used_idx)
    tibble(
      round = r,
      byes  = if (length(bye_idx) == 0) NA_character_
      else paste(p[bye_idx], collapse = ", ")
    )
  }) %>% dplyr::filter(!is.na(byes))
  if (nrow(out) == 0) return(NULL)
  out
}

# ---------- Pairing Logic (your exact templates) -------------------------------

make_schedule <- function(players) {
  n <- length(players); p <- players
  games <- NULL
  
  if (n == 4) {
    specs <- list(
      list(round=1, court=1, A=c(1,2), B=c(3,4)),
      list(round=2, court=1, A=c(1,3), B=c(2,4)),
      list(round=3, court=1, A=c(1,4), B=c(2,3))
    )
  } else if (n == 5) {
    specs <- list(
      list(round=1, court=1, A=c(1,2), B=c(3,5)),
      list(round=2, court=1, A=c(1,3), B=c(4,5)),
      list(round=3, court=1, A=c(2,5), B=c(3,4)),
      list(round=4, court=1, A=c(1,5), B=c(2,4)),
      list(round=5, court=1, A=c(1,4), B=c(2,3))
    )
  } else if (n == 6) {
    specs <- list(
      list(round=1, court=1, A=c(1,3), B=c(5,6)),
      list(round=2, court=1, A=c(1,2), B=c(3,4)),
      list(round=3, court=1, A=c(3,5), B=c(2,6)),
      list(round=4, court=1, A=c(1,5), B=c(2,4)),
      list(round=5, court=1, A=c(4,5), B=c(3,6)),
      list(round=6, court=1, A=c(1,6), B=c(2,5)),
      list(round=7, court=1, A=c(1,4), B=c(2,3))
    )
  } else if (n == 7) {
    specs <- list(
      list(round=1,  court=1, A=c(4,6), B=c(3,7)),
      list(round=2,  court=1, A=c(1,5), B=c(2,4)),
      list(round=3,  court=1, A=c(2,5), B=c(6,7)),
      list(round=4,  court=1, A=c(1,7), B=c(4,5)),
      list(round=5,  court=1, A=c(2,6), B=c(3,5)),
      list(round=6,  court=1, A=c(1,6), B=c(3,4)),
      list(round=7,  court=1, A=c(1,3), B=c(5,7)),
      list(round=8,  court=1, A=c(2,7), B=c(3,6)),
      list(round=9,  court=1, A=c(5,6), B=c(4,7)),
      list(round=10, court=1, A=c(1,4), B=c(2,3))
    )
  } else if (n == 8) {
    specs <- list(
      list(round=1, court=1, A=c(1,3), B=c(6,8)),
      list(round=1, court=2, A=c(2,4), B=c(5,7)),
      list(round=2, court=1, A=c(1,6), B=c(4,7)),
      list(round=2, court=2, A=c(3,8), B=c(2,5)),
      list(round=3, court=1, A=c(1,2), B=c(7,8)),
      list(round=3, court=2, A=c(3,4), B=c(5,6)),
      list(round=4, court=1, A=c(1,5), B=c(2,6)),
      list(round=4, court=2, A=c(4,8), B=c(3,7)),
      list(round=5, court=1, A=c(1,8), B=c(4,5)),
      list(round=5, court=2, A=c(2,7), B=c(3,6)),
      list(round=6, court=1, A=c(1,7), B=c(3,5)),
      list(round=6, court=2, A=c(4,6), B=c(2,8)),
      list(round=7, court=1, A=c(1,4), B=c(2,3)),
      list(round=7, court=2, A=c(6,7), B=c(5,8))
    )
  } else if (n == 9) {
    specs <- list(
      list(round=1,  court=2, A=c(4,9), B=c(5,8)),
      list(round=2,  court=1, A=c(1,2), B=c(8,9)),
      list(round=2,  court=2, A=c(3,4), B=c(5,7)),
      list(round=3,  court=1, A=c(1,3), B=c(6,8)),
      list(round=3,  court=2, A=c(2,5), B=c(7,9)),
      list(round=4,  court=1, A=c(1,9), B=c(3,7)),
      list(round=4,  court=2, A=c(2,8), B=c(4,6)),
      list(round=5,  court=1, A=c(2,9), B=c(3,8)),
      list(round=5,  court=2, A=c(4,7), B=c(5,6)),
      list(round=6,  court=1, A=c(1,8), B=c(4,5)),
      list(round=6,  court=2, A=c(2,7), B=c(3,6)),
      list(round=7,  court=1, A=c(1,7), B=c(2,6)),
      list(round=7,  court=2, A=c(3,9), B=c(4,8)),
      list(round=8,  court=1, A=c(1,5), B=c(2,4)),
      list(round=8,  court=2, A=c(6,9), B=c(7,8)),
      list(round=9,  court=1, A=c(1,4), B=c(2,3)),
      list(round=9,  court=2, A=c(5,9), B=c(6,7)),
      list(round=10, court=2, A=c(1,6), B=c(3,5))
    )
  } else if (n == 10) {
    specs <- list(
      list(round=1,  court=1, A=c(1,3), B=c(6,9)),
      list(round=1,  court=2, A=c(2,5), B=c(8,10)),
      list(round=2,  court=1, A=c(1,7), B=c(3,4)),
      list(round=2,  court=2, A=c(6,8), B=c(5,10)),
      list(round=3,  court=1, A=c(2,6), B=c(3,5)),
      list(round=3,  court=2, A=c(4,7), B=c(9,10)),
      list(round=4,  court=1, A=c(1,6), B=c(7,8)),
      list(round=4,  court=2, A=c(5,9), B=c(4,10)),
      list(round=5,  court=1, A=c(2,10), B=c(3,9)),
      list(round=5,  court=2, A=c(4,8),  B=c(5,7)),
      list(round=6,  court=1, A=c(1,9),  B=c(4,6)),
      list(round=6,  court=2, A=c(2,8),  B=c(3,7)),
      list(round=7,  court=1, A=c(1,10), B=c(3,8)),
      list(round=7,  court=2, A=c(2,9),  B=c(5,6)),
      list(round=8,  court=1, A=c(3,10), B=c(6,7)),
      list(round=8,  court=2, A=c(5,8),  B=c(4,9)),
      list(round=9,  court=1, A=c(1,8),  B=c(2,7)),
      list(round=9,  court=2, A=c(3,6),  B=c(4,5)),
      list(round=10, court=1, A=c(1,5),  B=c(2,4)),
      list(round=10, court=2, A=c(7,10), B=c(8,9)),
      list(round=11, court=1, A=c(1,4),  B=c(2,3)),
      list(round=11, court=2, A=c(6,10), B=c(7,9))
    )
  } else {
    shiny::validate(shiny::need(FALSE, "This app currently supports ranked schedules for 4–10 players."))
  }
  
  games <- mk_games_df(p, specs)
  byes_tbl <- compute_byes_from_games(p, games)
  list(
    games = games %>% arrange(round, court, game_id),
    byes  = byes_tbl
  )
}

# ---------- Scoring / Spreadsheet (by ROUND) ----------------------------------

player_game_results <- function(schedule_games, scores_tbl) {
  schedule_games %>%
    mutate(A_players = str_split(teamA, " / "),
           B_players = str_split(teamB, " / ")) %>%
    select(game_id, round, court, A_players, B_players) %>%
    pivot_longer(cols = c(A_players, B_players),
                 names_to = "team_label", values_to = "players") %>%
    mutate(team_label = ifelse(team_label == "A_players", "A", "B")) %>%
    unnest(players) %>%
    rename(player = players) %>%
    left_join(scores_tbl, by = "game_id") %>%
    mutate(
      diff = ifelse(is.na(scoreA) | is.na(scoreB), NA_real_, scoreA - scoreB),
      pdiff = case_when(
        is.na(diff) ~ NA_real_,
        team_label == "A" ~ diff,
        TRUE ~ -diff
      ),
      result = case_when(
        is.na(diff) ~ NA_character_,
        team_label == "A" & diff > 0 ~ "W",
        team_label == "B" & diff < 0 ~ "W",
        diff == 0 ~ "T",
        TRUE ~ "L"
      )
    )
}

compute_player_results <- function(players, schedule_games, scores_tbl) {
  game_results <- player_game_results(schedule_games, scores_tbl)
  round_cols_order <- paste0("Round ", sort(unique(schedule_games$round)))
  base <- tibble(Player = players)

  records <- game_results %>%
    group_by(player) %>%
    summarise(
      Wins = sum(result == "W", na.rm = TRUE),
      Losses = sum(result == "L", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      `Win %` = if_else(Wins + Losses > 0, Wins / (Wins + Losses), NA_real_)
    )

  rdiffs <- game_results %>%
    group_by(player, round) %>%
    summarise(
      rdiff = if (all(is.na(pdiff))) NA_real_ else sum(pdiff, na.rm = TRUE),
      .groups = "drop"
    )

  round_wide <- rdiffs %>%
    mutate(col = paste0("Round ", round)) %>%
    select(player, col, rdiff) %>%
    pivot_wider(names_from = col, values_from = rdiff)

  for (rc in setdiff(round_cols_order, names(round_wide))) round_wide[[rc]] <- NA_real_

  round_wide <- round_wide %>%
    select(player, all_of(round_cols_order))
  round_wide$Total <- rowSums(
    replace(round_wide[round_cols_order], is.na(round_wide[round_cols_order]), 0),
    na.rm = TRUE
  )

  base %>%
    left_join(records, by = c("Player" = "player")) %>%
    left_join(round_wide, by = c("Player" = "player")) %>%
    mutate(
      Wins = replace_na(Wins, 0L),
      Losses = replace_na(Losses, 0L)
    ) %>%
    select(Player, Wins, Losses, `Win %`, all_of(round_cols_order), Total)
}

extract_sheet_id <- function(url_or_id) {
  if (is.null(url_or_id) || !nzchar(trimws(url_or_id))) {
    env_id <- Sys.getenv("GOOGLE_SHEET_ID", "")
    if (nzchar(env_id)) return(env_id)
    stop("Enter a Google Sheet URL, or set GOOGLE_SHEET_ID in your environment.")
  }

  x <- trimws(url_or_id)
  if (!grepl("^https?://", x)) return(x)

  id <- stringr::str_match(x, "/d/([a-zA-Z0-9-_]+)")[, 2]
  if (!is.na(id)) return(id)

  id <- stringr::str_match(x, "[?&]id=([a-zA-Z0-9-_]+)")[, 2]
  if (!is.na(id)) return(id)

  stop("Could not parse a Google Sheet ID from that URL.")
}

gs_auth <- function() {
  sa <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (nzchar(sa)) {
    gs4_deauth()
    if (file.exists(sa)) {
      gs4_auth(path = sa)
    } else if (grepl("^\\s*\\{", sa)) {
      tmp <- tempfile(fileext = ".json")
      writeLines(sa, tmp)
      on.exit(unlink(tmp), add = TRUE)
      gs4_auth(path = tmp)
    } else {
      stop("GOOGLE_SERVICE_ACCOUNT_JSON must be a file path or JSON content.")
    }
    return(invisible(TRUE))
  }

  gs4_auth(email = TRUE)
  invisible(TRUE)
}

push_results_to_sheet <- function(results_tbl, sheet_id, sheet_name = "DDC Results") {
  gs_auth()
  sheet_write(results_tbl, ss = sheet_id, sheet = sheet_name)
  props <- sheet_properties(sheet_id)
  gid <- props$sheet_id[props$name == sheet_name]
  if (length(gid) == 1 && !is.na(gid)) {
    paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit#gid=", gid)
  } else {
    paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit")
  }
}

# ---------- UI ----------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .small-input input { max-width: 90px; }
    .score-box { display: inline-block; margin-right: 8px; }
    .round-header { background:#f6f6f6; padding:6px 10px; margin-top:10px; border-radius:6px; }
    .bye-line { font-style: italic; margin-left:0; margin-top:4px; }
  "))),
  # JS: heartbeat + LocalStorage save/restore
  tags$script(HTML("
    // Keep-alive: ping server every 25s
    setInterval(function(){ Shiny.setInputValue('keepalive', Math.random(), {priority:'event'}); }, 25000);

    // Save state sent from server
    Shiny.addCustomMessageHandler('saveState', function(state){
      try { localStorage.setItem('ddc_sched_state_v2', JSON.stringify(state)); } catch(e) {}
    });

    // Server asks for saved state
    Shiny.addCustomMessageHandler('requestState', function(x){
      var raw = localStorage.getItem('ddc_sched_state_v2');
      Shiny.setInputValue('saved_state', raw || null, {priority:'event'});
    });
  ")),
  titlePanel("DDC / Escape!! — Ranked Pairings & Scores (4–10 players)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Enter players in rank order (one per line)"),
      textAreaInput("players_raw", NULL,
                    placeholder = "Jim\nHank\nScott\nDrew\n…", rows = 10),
      actionButton("make_schedule", "Generate Schedule", class = "btn-primary"),
      br(), br(),
      h4("2) Options"),
      checkboxInput("compact_names", "Compact team display (initials)", FALSE),
      hr(),
      h4("Google Sheet"),
      textInput(
        "sheet_url",
        "Sheet URL or ID",
        placeholder = "https://docs.google.com/spreadsheets/d/...",
        width = "100%"
      ),
      actionButton("push_sheets", "Push Results to Google Sheet", class = "btn-success"),
      helpText(
        "Share the sheet with your Google account or service account before pushing.",
        tags$br(),
        "Optional env vars: GOOGLE_SHEET_ID, GOOGLE_SERVICE_ACCOUNT_JSON"
      ),
      verbatimTextOutput("sheet_push_status")
    ),
    mainPanel(
      h4("Schedule, Courts, Byes, & Scoring"),
      uiOutput("games_ui"),
      hr(),
      h4("Player Results"),
      DTOutput("player_table")
    )
  )
)

# ---------- SERVER ------------------------------------------------------------

server <- function(input, output, session) {
  session$allowReconnect(TRUE)  # allow automatic reconnect
  
  players_vec <- reactive({
    req(input$players_raw)
    x <- str_split(input$players_raw, "\\r?\\n")[[1]] |> str_trim()
    x <- x[nzchar(x)]
    shiny::validate(shiny::need(length(x) >= 4, "Please enter at least 4 players (up to 10 supported)."))
    shiny::validate(shiny::need(length(x) <= 10, "Please limit to 10 players for this version."))
    x
  })
  
  schedule <- eventReactive(input$make_schedule, {
    make_schedule(players_vec())
  }, ignoreInit = TRUE)
  
  # team label helper
  team_label <- function(txt) {
    if (!isTRUE(input$compact_names)) return(txt)
    ppl <- str_split(txt, " / ")[[1]]
    paste(str_replace_all(ppl, "(\\b\\w)\\w*", "\\1."), collapse = " / ")
  }
  
  # Collect scores directly from inputs (updates instantly as you type)
  scores_tbl <- reactive({
    req(schedule())
    gms <- schedule()$games
    pull_val <- function(id) { v <- input[[id]]; if (is.null(v)) NA_real_ else as.numeric(v) }
    tibble(
      game_id = gms$game_id,
      scoreA  = sapply(gms$game_id, function(gid) pull_val(paste0("A_", gid))),
      scoreB  = sapply(gms$game_id, function(gid) pull_val(paste0("B_", gid)))
    )
  })
  
  # AUTO-SAVE state to LocalStorage whenever players/scores/schedule change
  observe({
    if (is.null(schedule())) return()
    st <- list(
      players_raw = if (!is.null(input$players_raw)) input$players_raw else "",
      scores = scores_tbl() |> as.list()  # list of columns: game_id, scoreA, scoreB
    )
    session$sendCustomMessage("saveState", st)
  })
  
  # On connect / refresh: ask browser for saved state
  observe({
    session$sendCustomMessage("requestState", TRUE)
  })
  
  # When saved state arrives, restore players and (after schedule) the scores
  observeEvent(input$saved_state, {
    if (is.null(input$saved_state) || !nzchar(input$saved_state)) return()
    dat <- tryCatch(jsonlite::fromJSON(input$saved_state), error=function(e) NULL)
    if (is.null(dat)) return()
    
    # Restore players text if empty
    if (!is.null(dat$players_raw) && nzchar(dat$players_raw) &&
        (is.null(input$players_raw) || !nzchar(input$players_raw))) {
      updateTextAreaInput(session, "players_raw", value = dat$players_raw)
    }
    
    # After schedule is generated, restore scores to inputs
    observeEvent(schedule(), {
      if (!is.null(dat$scores)) {
        ids <- schedule()$games$game_id
        for (i in seq_along(dat$scores$game_id)) {
          gid <- dat$scores$game_id[i]
          if (gid %in% ids) {
            a <- dat$scores$scoreA[i]; b <- dat$scores$scoreB[i]
            if (!is.na(a)) updateNumericInput(session, paste0("A_", gid), value = a)
            if (!is.na(b)) updateNumericInput(session, paste0("B_", gid), value = b)
          }
        }
      }
    }, once = TRUE)
  }, ignoreInit = FALSE)
  
  # Build the schedule UI (with byes under Team B)
  output$games_ui <- renderUI({
    req(schedule())
    sch  <- schedule()$games
    byes <- schedule()$byes  # tibble(round, byes) or NULL
    
    rounds <- sort(unique(sch$round))
    elems <- list()
    
    for (r in rounds) {
      sub <- sch %>% dplyr::filter(round == r) %>% dplyr::arrange(court)
      elems <- append(elems, list(div(class="round-header", strong(sprintf("Round %d", r)))))
      
      round_byes <- NULL
      if (!is.null(byes)) {
        b <- byes %>% dplyr::filter(round == r)
        if (nrow(b) == 1 && !is.na(b$byes) && nzchar(b$byes)) round_byes <- paste0("Byes: ", b$byes)
      }
      
      for (i in seq_len(nrow(sub))) {
        gid <- sub$game_id[i]
        elems <- append(elems, list(
          fluidRow(
            column(
              7,
              strong(sprintf("Game %d (Court %d):", gid, sub$court[i])),
              div(sprintf("Team A: %s", team_label(sub$teamA[i]))),
              div(sprintf("Team B: %s", team_label(sub$teamB[i]))),
              if (!is.null(round_byes)) div(class="bye-line", round_byes)
            ),
            column(
              5,
              div(class = "score-box small-input",
                  numericInput(paste0("A_", gid), "A", value = NA, min = 0, step = 1)
              ),
              div(class = "score-box small-input",
                  numericInput(paste0("B_", gid), "B", value = NA, min = 0, step = 1)
              )
            )
          ),
          hr()
        ))
      }
    }
    do.call(tagList, elems)
  })
  
  player_results <- reactive({
    req(schedule())
    compute_player_results(players_vec(), schedule()$games, scores_tbl())
  })

  output$player_table <- renderDT({
    req(player_results())
    tbl <- player_results()
    datatable(
      tbl,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(dom = "Bfrtip", buttons = c("copy", "csv"), pageLength = 25)
    ) %>%
      formatPercentage("Win %", digits = 1)
  })

  output$sheet_push_status <- renderText({
    sheet_push_status()
  })

  sheet_push_status <- reactiveVal("")

  observeEvent(input$push_sheets, {
    sheet_push_status("Pushing results to Google Sheet...")
    result <- tryCatch({
      tbl <- player_results()
      req(nrow(tbl) > 0)
      ss_id <- extract_sheet_id(input$sheet_url)
      sheet_url <- push_results_to_sheet(tbl, ss_id)
      list(ok = TRUE, msg = paste0("Results pushed successfully.\n", sheet_url))
    }, error = function(e) {
      list(ok = FALSE, msg = paste0("Push failed: ", conditionMessage(e)))
    })

    if (result$ok) {
      showNotification("Results pushed to Google Sheet.", type = "message", duration = 5)
    } else {
      showNotification(result$msg, type = "error", duration = 8)
    }
    sheet_push_status(result$msg)
  }, ignoreInit = TRUE)
  
  # No-op observer just to register keepalive input (so it shows as active)
  observeEvent(input$keepalive, { }, ignoreInit = TRUE)
}

shinyApp(ui, server)
