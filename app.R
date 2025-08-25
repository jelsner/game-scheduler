# app.R
# DDC / Escape!! ranked pairings + scoring
# Supports N = 4..10 with your exact matchups (5..10 as provided).
# Shows byes (auto-computed), supports two courts (8–10), builds per-player point-diff sheet.

library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(DT)

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

# ---------- Pairing Logic (exactly as provided where applicable) ---------------

make_schedule <- function(players) {
  n <- length(players); p <- players
  games <- NULL
  
  if (n == 4) {
    specs <- list(
      list(round=1, court=1, A=c(1,2), B=c(3,4)),
      list(round=2, court=1, A=c(1,3), B=c(2,4)),
      list(round=3, court=1, A=c(1,4), B=c(2,3))
    )
    games <- mk_games_df(p, specs)
    
  } else if (n == 5) {
    specs <- list(
      list(round=1, court=1, A=c(1,2), B=c(3,5)),
      list(round=2, court=1, A=c(1,3), B=c(4,5)),
      list(round=3, court=1, A=c(2,5), B=c(3,4)),
      list(round=4, court=1, A=c(1,5), B=c(2,4)),
      list(round=5, court=1, A=c(1,4), B=c(2,3))
    )
    games <- mk_games_df(p, specs)
    
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
    games <- mk_games_df(p, specs)
    
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
    games <- mk_games_df(p, specs)
    
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
    games <- mk_games_df(p, specs)
    
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
    games <- mk_games_df(p, specs)
    
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
    games <- mk_games_df(p, specs)
    
  } else {
    validate(need(FALSE, "This app currently supports ranked schedules for 4–10 players."))
  }
  
  byes_tbl <- compute_byes_from_games(p, games)
  list(
    games = games %>% arrange(round, court, game_id),
    byes  = byes_tbl
  )
}

# ---------- Scoring / Spreadsheet ---------------------------------------------

# Per-player point differentials by ROUND (not per game).
# For rounds with two courts (e.g., N >= 8), a player still appears in only one game,
# so the round value is just that game’s diff. If a player sits out (bye), the round is NA.
compute_player_diffs <- function(players, schedule_games, scores_tbl) {
  # Expand schedule to long: game_id, round, team_label, player
  long_sched <- schedule_games %>%
    mutate(A_players = str_split(teamA, " / "),
           B_players = str_split(teamB, " / ")) %>%
    select(game_id, round, court, A_players, B_players) %>%
    pivot_longer(cols = c(A_players, B_players),
                 names_to = "team_label", values_to = "players") %>%
    mutate(team_label = ifelse(team_label == "A_players", "A", "B")) %>%
    unnest(players) %>%
    rename(player = players)
  
  # Join scores (diff = A - B; A gets +diff; B gets -diff)
  diffs_long <- long_sched %>%
    left_join(scores_tbl, by = "game_id") %>%
    mutate(
      diff  = ifelse(is.na(scoreA) | is.na(scoreB), NA_real_, scoreA - scoreB),
      pdiff = case_when(
        is.na(diff) ~ NA_real_,
        team_label == "A" ~  diff,
        TRUE              ~ -diff
      )
    ) %>%
    select(player, round, pdiff)
  
  # Collapse to per-ROUND value per player
  # (If a player could ever appear twice in one round, we’d sum; otherwise it’s just that one game.)
  rdiffs <- diffs_long %>%
    group_by(player, round) %>%
    summarise(
      rdiff = if (all(is.na(pdiff))) NA_real_ else sum(pdiff, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Build wide table with ROUND columns
  base <- tibble(Player = players)
  round_cols_order <- paste0("Round ", sort(unique(schedule_games$round)))
  
  wide <- rdiffs %>%
    mutate(col = paste0("Round ", round)) %>%
    select(player, col, rdiff) %>%
    pivot_wider(names_from = col, values_from = rdiff) %>%
    right_join(base, by = c("player" = "Player")) %>%
    rename(Player = player)
  
  # Ensure round columns are in numeric order and present even if entirely NA
  for (rc in setdiff(round_cols_order, names(wide))) {
    wide[[rc]] <- NA_real_
  }
  wide <- wide %>% select(Player, all_of(round_cols_order))
  
  # Total: treat NA as 0 when summing
  wide$Total <- rowSums(replace(wide[round_cols_order], is.na(wide[round_cols_order]), 0), na.rm = TRUE)
  
  wide
}


# ---------- UI ----------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .small-input input { max-width: 90px; }
    .score-box { display: inline-block; margin-right: 8px; }
    .round-header { background:#f6f6f6; padding:6px 10px; margin-top:10px; border-radius:6px; }
    .bye-line { font-style: italic; margin-left:0; margin-top:4px; }
  "))),
  titlePanel("DDC / Escape!! — Ranked Pairings & Scores (4–10 players)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1) Enter players in rank order (one per line)"),
      textAreaInput(
        "players_raw", NULL,
        placeholder = "Jim\nHank\nScott\nDrew\n…",
        rows = 10
      ),
      actionButton("make_schedule", "Generate Schedule", class = "btn-primary"),
      br(), br(),
      h4("2) Options"),
      checkboxInput("compact_names", "Compact team display (initials)", FALSE),
      hr(),
      h4("Download"),
      downloadButton("download_csv", "Download Player Differentials (CSV)")
    ),
    
    mainPanel(
      h4("Schedule, Courts, Byes, & Scoring"),
      uiOutput("games_ui"),
      hr(),
      h4("Per-Player Point Differentials"),
      DTOutput("player_table")
    )
  )
)

# ---------- SERVER ------------------------------------------------------------

server <- function(input, output, session) {
  
  players_vec <- reactive({
    req(input$players_raw)
    x <- str_split(input$players_raw, "\\r?\\n")[[1]] |> str_trim()
    x <- x[nzchar(x)]
    validate(need(length(x) >= 4, "Please enter at least 4 players (up to 10 supported)."))
    validate(need(length(x) <= 10, "Please limit to 10 players for this version."))
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
  
  
  # Schedule UI grouped by round; show byes (if any); 1 or 2 courts per round
  output$games_ui <- renderUI({
    req(schedule())
    sch  <- schedule()$games
    byes <- schedule()$byes  # tibble(round, byes) or NULL
    
    rounds <- sort(unique(sch$round))
    elems <- list()
    
    for (r in rounds) {
      sub <- sch %>% dplyr::filter(round == r) %>% dplyr::arrange(court)
      elems <- append(elems, list(div(class="round-header", strong(sprintf("Round %d", r)))))
      
      # Pull bye text for this round (single string like "P1, P2" or NA/empty)
      round_byes <- NULL
      if (!is.null(byes)) {
        b <- byes %>% dplyr::filter(round == r)
        if (nrow(b) == 1 && !is.na(b$byes) && nzchar(b$byes)) {
          round_byes <- paste0("Byes: ", b$byes)
        }
      }
      
      # games in this round
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
  
  # Save buttons to update rv_scores
  observeEvent(schedule(), {
    req(schedule())
    gms <- schedule()$games
    lapply(gms$game_id, function(gid) {
      observeEvent(input[[paste0("save_", gid)]], {
        sc <- rv_scores()
        idx <- which(sc$game_id == gid)
        sc$scoreA[idx] <- input[[paste0("A_", gid)]]
        sc$scoreB[idx] <- input[[paste0("B_", gid)]]
        rv_scores(sc)
      }, ignoreInit = TRUE)
    })
  }, ignoreInit = TRUE)
  
  # Collect scores directly from the numeric inputs for each game
  scores_tbl <- reactive({
    req(schedule())
    gms <- schedule()$games
    # read A_#, B_# inputs, coerce NULL -> NA_real_
    pull_val <- function(id) {
      v <- input[[id]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }
    tibble(
      game_id = gms$game_id,
      scoreA  = sapply(gms$game_id, function(gid) pull_val(paste0("A_", gid))),
      scoreB  = sapply(gms$game_id, function(gid) pull_val(paste0("B_", gid)))
    )
  })
  
  # Player diffs table
  player_diffs <- reactive({
    req(schedule())
    gms <- schedule()$games
    sc  <- scores_tbl()  # <<-- use the inputs directly
    compute_player_diffs(players_vec(), gms, sc)
  })
  
  output$player_table <- renderDT({
    req(player_diffs())
    datatable(
      player_diffs(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv"),
        pageLength = 25
      )
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("player_differentials_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) readr::write_csv(player_diffs(), file)
  )
}

shinyApp(ui, server)
