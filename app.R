# app.R
# DDC / Escape!! ranked pairings + scoring (4–11 players)
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

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ---------- Helpers ------------------------------------------------------------

google_service_account_email <- function() {
  sa <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (!nzchar(sa)) sa <- "secrets/game-scheduler-service-account.json"
  if (file.exists(sa)) {
    email <- tryCatch(jsonlite::fromJSON(sa)$client_email, error = function(e) NULL)
    if (!is.null(email) && nzchar(email)) return(email)
  }
  "ddc-scheduler@game-scheduler-501709.iam.gserviceaccount.com"
}

short_player_name <- function(name) {
  purrr::map_chr(name, function(x) {
    parts <- str_split(str_squish(x), "\\s+")[[1]]
    if (length(parts) < 2) return(parts[1])
    paste(parts[1], paste0(str_sub(parts[2], 1, 1), "."))
  })
}

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
      teamB = purrr::map_chr(B_idx, ~ paste(p[.x], collapse = " / ")),
      teamA_display = purrr::map_chr(A_idx, ~ paste(short_player_name(p[.x]), collapse = " / ")),
      teamB_display = purrr::map_chr(B_idx, ~ paste(short_player_name(p[.x]), collapse = " / ")),
      count_in_standings = TRUE
    ) %>%
    select(game_id, round, court, A_idx, B_idx, teamA, teamB, teamA_display, teamB_display, count_in_standings)
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
      else paste(short_player_name(p[bye_idx]), collapse = ", ")
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
  } else if (n == 11) {
    specs <- list(
      list(round=1,  court=1, A=c(1,3),  B=c(9,11)),
      list(round=2,  court=1, A=c(1,8),  B=c(4,5)),
      list(round=2,  court=2, A=c(2,7),  B=c(3,6)),
      list(round=3,  court=1, A=c(8,11), B=c(9,10)),
      list(round=3,  court=2, A=c(1,7),  B=c(2,5)),
      list(round=4,  court=1, A=c(7,11), B=c(8,10)),
      list(round=4,  court=2, A=c(1,6),  B=c(3,4)),
      list(round=5,  court=1, A=c(4,9),  B=c(10,11)),
      list(round=5,  court=2, A=c(2,6),  B=c(3,5)),
      list(round=6,  court=1, A=c(7,10), B=c(8,9)),
      list(round=6,  court=2, A=c(1,5),  B=c(2,4)),
      list(round=7,  court=1, A=c(3,9),  B=c(4,7)),
      list(round=7,  court=2, A=c(5,11), B=c(6,10)),
      list(round=8,  court=1, A=c(1,11), B=c(4,8)),
      list(round=8,  court=2, A=c(2,10), B=c(5,7)),
      list(round=9,  court=1, A=c(3,11), B=c(4,10)),
      list(round=9,  court=2, A=c(5,9),  B=c(6,8)),
      list(round=10, court=1, A=c(1,9),  B=c(3,7)),
      list(round=10, court=2, A=c(2,8),  B=c(4,6)),
      list(round=11, court=1, A=c(2,11), B=c(6,7)),
      list(round=11, court=2, A=c(3,10), B=c(5,8)),
      list(round=12, court=1, A=c(1,10), B=c(5,6)),
      list(round=12, court=2, A=c(2,9),  B=c(3,8)),
      list(round=13, court=1, A=c(4,11), B=c(6,9)),
      list(round=13, court=2, A=c(5,10), B=c(7,8)),
      list(round=14, court=1, A=c(6,11), B=c(7,9)),
      list(round=14, court=2, A=c(1,4),  B=c(2,3))
    )
  } else {
    shiny::validate(shiny::need(FALSE, "This app currently supports ranked schedules for 4-11 players."))
  }
  
  games <- mk_games_df(p, specs)
  byes_tbl <- compute_byes_from_games(p, games)
  list(
    games = games %>% arrange(round, court, game_id),
    byes  = byes_tbl
  )
}

add_schedule_context <- function(schedule, phase, group, game_offset = 0, court_offset = 0) {
  games <- schedule$games %>%
    mutate(
      game_id = game_id + game_offset,
      court = court + court_offset,
      phase = phase,
      group = group,
      count_in_standings = count_in_standings %||% TRUE
    ) %>%
    select(game_id, phase, group, round, court, dplyr::everything())

  byes <- schedule$byes
  if (!is.null(byes)) {
    byes <- byes %>%
      mutate(phase = phase, group = group) %>%
      select(phase, group, round, byes)
  }

  list(games = games, byes = byes)
}

make_manual_game <- function(game_id, round_number, team_a, team_b, count_in_standings) {
  tibble(
    game_id = game_id,
    phase = "Manual",
    group = "Extra Games",
    round = round_number,
    court = 1L,
    A_idx = I(list(integer())),
    B_idx = I(list(integer())),
    teamA = paste(team_a, collapse = " / "),
    teamB = paste(team_b, collapse = " / "),
    teamA_display = paste(short_player_name(team_a), collapse = " / "),
    teamB_display = paste(short_player_name(team_b), collapse = " / "),
    count_in_standings = count_in_standings
  )
}

empty_manual_games <- function() {
  tibble(
    game_id = integer(),
    phase = character(),
    group = character(),
    round = integer(),
    court = integer(),
    A_idx = I(list()),
    B_idx = I(list()),
    teamA = character(),
    teamB = character(),
    teamA_display = character(),
    teamB_display = character(),
    count_in_standings = logical()
  )
}

bind_byes <- function(...) {
  byes <- bind_rows(...)
  if (nrow(byes) == 0 || !all(c("phase", "group", "round", "byes") %in% names(byes))) {
    return(NULL)
  }
  byes
}

snake_groups <- function(players) {
  n <- length(players)
  group_sizes <- c(floor(n / 2), ceiling(n / 2))
  groups <- list(character(), character())

  for (seed in seq_along(players)) {
    row <- ceiling(seed / 2)
    order <- if (row %% 2 == 1) c(1, 2) else c(2, 1)
    target_group <- if (seed %% 2 == 1) order[1] else order[2]

    if (length(groups[[target_group]]) >= group_sizes[target_group]) {
      target_group <- 3 - target_group
    }

    groups[[target_group]] <- c(groups[[target_group]], players[seed])
  }

  names(groups) <- c("Group A", "Group B")
  groups
}

make_two_group_prelim_schedule <- function(players) {
  groups <- snake_groups(players)
  parts <- list()
  game_offset <- 0

  for (idx in seq_along(groups)) {
    sch <- make_schedule(groups[[idx]])
    part <- add_schedule_context(
      sch,
      phase = "Preliminary",
      group = names(groups)[idx],
      game_offset = game_offset,
      court_offset = idx - 1
    )
    parts[[idx]] <- part
    game_offset <- max(part$games$game_id)
  }

  list(
    games = bind_rows(purrr::map(parts, "games")) %>% arrange(phase, round, court, game_id),
    byes = bind_byes(purrr::map(parts, "byes")),
    groups = groups
  )
}

score_input_id <- function(prefix, gid, version) {
  paste0(prefix, "_", version, "_", gid)
}

scores_for_games <- function(games, input, version) {
  pull_val <- function(id) {
    v <- input[[id]]
    if (is.null(v)) NA_real_ else as.numeric(v)
  }

  tibble(
    game_id = games$game_id,
    scoreA = sapply(games$game_id, function(gid) pull_val(score_input_id("A", gid, version))),
    scoreB = sapply(games$game_id, function(gid) pull_val(score_input_id("B", gid, version)))
  )
}

games_complete <- function(scores_tbl) {
  nrow(scores_tbl) > 0 && all(!is.na(scores_tbl$scoreA) & !is.na(scores_tbl$scoreB))
}

players_in_games <- function(games, preferred_order = character()) {
  game_players <- unique(unlist(str_split(c(games$teamA, games$teamB), " / ")))
  game_players <- game_players[nzchar(game_players)]
  c(preferred_order[preferred_order %in% game_players], setdiff(game_players, preferred_order))
}

ranked_players_for_group <- function(players, games, scores_tbl) {
  compute_player_results(players, games, scores_tbl) %>% pull(Player)
}

make_two_group_finals_schedule <- function(prelim_schedule, prelim_scores, prelim_results = NULL) {
  if (!games_complete(prelim_scores)) return(NULL)

  group_names <- names(prelim_schedule$groups)
  if (is.null(prelim_results)) {
    rankings <- purrr::map(group_names, function(group_name) {
      group_games <- prelim_schedule$games %>% filter(group == group_name)
      group_scores <- prelim_scores %>% semi_join(group_games, by = "game_id")
      ranked_players_for_group(prelim_schedule$groups[[group_name]], group_games, group_scores)
    })
  } else {
    rankings <- purrr::map(group_names, function(group_name) {
      prelim_results %>% filter(Group == group_name) %>% pull(Player)
    })
  }
  names(rankings) <- group_names

  championship_players <- c(rankings[[1]][1], rankings[[2]][1], rankings[[1]][2], rankings[[2]][2])
  consolation_players <- character()
  max_rank <- max(length(rankings[[1]]), length(rankings[[2]]))
  for (rank in seq(3, max_rank)) {
    if (rank <= length(rankings[[1]])) consolation_players <- c(consolation_players, rankings[[1]][rank])
    if (rank <= length(rankings[[2]])) consolation_players <- c(consolation_players, rankings[[2]][rank])
  }

  game_offset <- max(prelim_schedule$games$game_id)
  championship <- add_schedule_context(
    make_schedule(championship_players),
    phase = "Finals",
    group = "Championship Final",
    game_offset = game_offset,
    court_offset = 0
  )

  consolation <- add_schedule_context(
    make_schedule(consolation_players),
    phase = "Finals",
    group = "Consolation Final",
    game_offset = max(championship$games$game_id),
    court_offset = 1
  )

  list(
    games = bind_rows(championship$games, consolation$games) %>% arrange(phase, round, court, game_id),
    byes = bind_byes(championship$byes, consolation$byes),
    championship_players = championship_players,
    consolation_players = consolation_players
  )
}

offset_numeric_ranks <- function(tbl, offset) {
  tbl %>%
    mutate(
      Rank = case_when(
        Rank == "Shootout" ~ Rank,
        TRUE ~ as.character(suppressWarnings(as.integer(Rank)) + offset)
      )
    )
}

compute_two_group_results <- function(prelim_schedule, prelim_scores, finals_schedule, all_scores) {
  if (is.null(finals_schedule)) {
    group_names <- names(prelim_schedule$groups)
    return(bind_rows(purrr::map(group_names, function(group_name) {
      group_games <- prelim_schedule$games %>% filter(group == group_name)
      group_scores <- prelim_scores %>% semi_join(group_games, by = "game_id")
      compute_player_results(prelim_schedule$groups[[group_name]], group_games, group_scores) %>%
        mutate(Group = group_name) %>%
        select(Group, dplyr::everything())
    })))
  }

  championship_games <- finals_schedule$games %>% filter(group == "Championship Final")
  championship_scores <- all_scores %>% semi_join(championship_games, by = "game_id")
  consolation_games <- finals_schedule$games %>% filter(group == "Consolation Final")
  consolation_scores <- all_scores %>% semi_join(consolation_games, by = "game_id")

  championship <- compute_player_results(
    finals_schedule$championship_players,
    championship_games,
    championship_scores
  ) %>%
    mutate(Group = "Championship Final") %>%
    select(Group, dplyr::everything())

  consolation <- compute_player_results(
    finals_schedule$consolation_players,
    consolation_games,
    consolation_scores
  ) %>%
    offset_numeric_ranks(4) %>%
    mutate(Group = "Consolation Final") %>%
    select(Group, dplyr::everything())

  bind_rows(championship, consolation)
}

find_shootout_groups <- function(results_tbl) {
  if (!"Rank" %in% names(results_tbl) || !"Player" %in% names(results_tbl)) return(list())

  group_cols <- intersect(c("Group", "Wins", "Losses", "Win %", "Total"), names(results_tbl))
  groups_tbl <- results_tbl %>%
    mutate(
      .row_id = row_number(),
      .seed = if ("Seed" %in% names(.)) Seed else row_number()
    ) %>%
    filter(Rank == "Shootout") %>%
    group_by(across(all_of(group_cols))) %>%
    filter(n() >= 2) %>%
    summarise(
      rows = list(.row_id),
      players = list(Player),
      seeds = list(.seed),
      .groups = "drop"
    )

  if (nrow(groups_tbl) == 0) return(list())

  groups_tbl %>%
    mutate(
      label = if ("Group" %in% names(.)) {
        paste0(Group, " | ", Wins, " wins | ", Total, " point diff")
      } else {
        paste0(Wins, " wins | ", Total, " point diff")
      },
      id = paste0("shootout_", str_replace_all(str_to_lower(label), "[^a-z0-9]+", "_"), row_number())
    ) %>%
    split(seq_len(nrow(.)))
}

apply_shootout_resolutions <- function(results_tbl, shootout_groups, input) {
  if (length(shootout_groups) == 0) return(results_tbl)

  out <- results_tbl
  for (group_info in shootout_groups) {
    rows <- unlist(group_info$rows)
    players <- unlist(group_info$players)
    seeds <- unlist(group_info$seeds)
    players <- players[order(seeds)]

    if (length(players) == 3) {
      top_seed <- players[1]
      lower_seeds <- players[2:3]
      first_winner <- input[[paste0(group_info$id, "_first_winner")]]
      final_winner <- input[[paste0(group_info$id, "_final_winner")]]
      if (is.null(first_winner) || is.null(final_winner) ||
          !nzchar(first_winner) || !nzchar(final_winner) ||
          !(first_winner %in% lower_seeds) ||
          !(final_winner %in% c(top_seed, first_winner))) {
        next
      }
      first_loser <- setdiff(lower_seeds, first_winner)
      final_loser <- setdiff(c(top_seed, first_winner), final_winner)
      resolved_players <- c(final_winner, final_loser, first_loser)
    } else {
      winner <- input[[paste0(group_info$id, "_winner")]]
      loser <- input[[paste0(group_info$id, "_loser")]]
      if (is.null(winner) || is.null(loser) || !nzchar(winner) || !nzchar(loser) || winner == loser) {
        next
      }

      middle <- players[!(players %in% c(winner, loser))]
      resolved_players <- c(winner, middle, loser)
    }

    out[rows, ] <- out[match(resolved_players, out$Player), ]
    if ("Group" %in% names(out)) {
      group_name <- out$Group[rows[1]]
      group_rows <- which(out$Group == group_name)
      start_rank <- match(min(rows), group_rows)
    } else {
      start_rank <- min(rows)
    }
    out$Rank[rows] <- as.character(seq(start_rank, start_rank + length(rows) - 1))
  }

  out
}

unresolved_shootout_labels <- function(shootout_groups, input) {
  purrr::keep(shootout_groups, function(group_info) {
    players <- unlist(group_info$players)
    seeds <- unlist(group_info$seeds)
    players <- players[order(seeds)]

    if (length(players) == 3) {
      top_seed <- players[1]
      lower_seeds <- players[2:3]
      first_winner <- input[[paste0(group_info$id, "_first_winner")]]
      final_winner <- input[[paste0(group_info$id, "_final_winner")]]
      return(
        is.null(first_winner) || is.null(final_winner) ||
          !nzchar(first_winner) || !nzchar(final_winner) ||
          !(first_winner %in% lower_seeds) ||
          !(final_winner %in% c(top_seed, first_winner))
      )
    }

    winner <- input[[paste0(group_info$id, "_winner")]]
    loser <- input[[paste0(group_info$id, "_loser")]]
    is.null(winner) || is.null(loser) || !nzchar(winner) || !nzchar(loser) || winner == loser
  }) %>%
    purrr::map_chr("label")
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
  base <- tibble(Player = players, Seed = seq_along(players))

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

  tbl <- base %>%
    left_join(records, by = c("Player" = "player")) %>%
    left_join(round_wide, by = c("Player" = "player")) %>%
    mutate(
      Player = short_player_name(Player),
      Wins = replace_na(Wins, 0L),
      Losses = replace_na(Losses, 0L)
    ) %>%
    select(Player, Seed, Wins, Losses, `Win %`, all_of(round_cols_order), Total)

  add_final_ranking(tbl)
}

add_final_ranking <- function(tbl) {
  ranked <- tbl %>%
    mutate(
      .win = replace_na(`Win %`, -1),
      .tot = replace_na(Total, -Inf)
    ) %>%
    arrange(desc(.win), desc(.tot), Player) %>%
    mutate(.pos = row_number())

  tie_info <- ranked %>%
    group_by(.win, .tot) %>%
    summarise(.start_pos = min(.pos), .n = n(), .groups = "drop")

  ranked %>%
    left_join(tie_info, by = c(".win", ".tot")) %>%
    mutate(
      Rank = if_else(.n > 1L, "Shootout", as.character(.start_pos))
    ) %>%
    select(Rank, Player, Seed, Wins, Losses, `Win %`, dplyr::everything(), -`.win`, -`.tot`, -`.pos`, -`.start_pos`, -`.n`)
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

google_sheet_url <- function(url_or_id) {
  sheet_id <- extract_sheet_id(url_or_id)
  paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit")
}

gs_auth <- function() {
  sa <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "")
  if (!nzchar(sa)) {
    sa <- "secrets/game-scheduler-service-account.json"
  }

  gs4_deauth()

  if (file.exists(sa)) {
    gs4_auth(path = sa, cache = FALSE)
    return(invisible(TRUE))
  }

  if (grepl("^\\s*\\{", sa)) {
    creds <- jsonlite::fromJSON(sa, simplifyVector = FALSE)
    gs4_auth(
      credentials = gargle::credentials_service_account(info = creds),
      cache = FALSE
    )
    return(invisible(TRUE))
  }

  if (interactive()) {
    gs4_auth(email = TRUE)
    return(invisible(TRUE))
  }

  stop(
    "Google Sheets credentials are not configured on the server. ",
    "Add secrets/game-scheduler-service-account.json and a project .Renviron, then redeploy.",
    call. = FALSE
  )
}

push_results_to_sheet <- function(results_tbl, sheet_id, sheet_name = "DDC Results") {
  gs_auth()
  sheet_write(results_tbl, ss = sheet_id, sheet = sheet_name)
  format_results_sheet(sheet_id, sheet_name, results_tbl)
  props <- sheet_properties(sheet_id)
  gid <- props$sheet_id[props$name == sheet_name]
  if (length(gid) == 1 && !is.na(gid)) {
    paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit#gid=", gid)
  } else {
    paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit")
  }
}

push_result_tabs_to_sheet <- function(tabs, sheet_id) {
  gs_auth()
  purrr::iwalk(tabs, function(tbl, sheet_name) {
    sheet_write(tbl, ss = sheet_id, sheet = sheet_name)
    format_results_sheet(sheet_id, sheet_name, tbl)
  })
  paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/edit")
}

format_results_sheet <- function(sheet_id, sheet_name, results_tbl) {
  col_letter <- function(idx) {
    letters <- character()
    while (idx > 0) {
      rem <- (idx - 1L) %% 26L
      letters <- c(LETTERS[rem + 1L], letters)
      idx <- (idx - rem - 1L) %/% 26L
    }
    paste0(letters, collapse = "")
  }

  col_range <- function(col_name) {
    letter <- col_letter(match(col_name, names(results_tbl)))
    paste0(letter, ":", letter)
  }

  apply_format <- function(range, ...) {
    cell <- googlesheets4:::CellData(
      userEnteredFormat = do.call(
        googlesheets4:::new,
        c(list("CellFormat"), list(...))
      )
    )
    range_flood(sheet_id, sheet = sheet_name, range = range, cell = cell)
  }

  apply_format(
    "1:1",
    horizontalAlignment = "CENTER",
    textFormat = list(bold = TRUE)
  )

  text_cols <- intersect(c("Group", "Player"), names(results_tbl))
  for (col in text_cols) {
    apply_format(
      col_range(col),
      horizontalAlignment = "LEFT"
    )
  }

  centered_cols <- setdiff(names(results_tbl), text_cols)
  for (col in centered_cols) {
    apply_format(
      col_range(col),
      horizontalAlignment = "CENTER"
    )
  }

  if ("Win %" %in% names(results_tbl)) {
    apply_format(
      col_range("Win %"),
      horizontalAlignment = "CENTER",
      numberFormat = list(type = "PERCENT", pattern = "0.00%")
    )
  }

  numeric_cols <- setdiff(
    names(results_tbl)[vapply(results_tbl, is.numeric, logical(1))],
    "Win %"
  )
  for (col in numeric_cols) {
    apply_format(
      col_range(col),
      horizontalAlignment = "CENTER",
      numberFormat = list(type = "NUMBER", pattern = "0")
    )
  }

  range_autofit(sheet_id, sheet = sheet_name, dimension = "columns")
  invisible(TRUE)
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
  titlePanel("DDC / Escape!! - Ranked Pairings & Scores (4-11 players)"),
  tags$p(
    class = "text-muted",
    "Ranked Pairings are based on the Kodiak system developed by Cade Loving."
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter players in seeded order (one per line)"),
      textAreaInput("players_raw", NULL,
                    placeholder = "Jim\nHank\nScott\nDrew\n…", rows = 10),
      radioButtons(
        "format_mode",
        "Format",
        choices = c(
          "Single MoC" = "single",
          "Two groups + finals" = "two_group"
        ),
        selected = "single"
      ),
      actionButton("make_schedule", "Generate Schedule", class = "btn-primary"),
      uiOutput("finals_controls"),
      br(), br(),
      hr(),
      h4("Scorekeeper"),
      passwordInput("admin_password", "Admin password"),
      actionButton("unlock_admin", "Unlock scorekeeping", class = "btn-warning"),
      uiOutput("admin_status"),
      hr(),
      h4("Results"),
      textInput(
        "sheet_url",
        "Sheet URL or ID",
        placeholder = "https://docs.google.com/spreadsheets/d/...",
        width = "100%"
      ),
      uiOutput("check_results_ui"),
      actionButton("push_sheets", "Push Results", class = "btn-success"),
      helpText(
        tags$strong("To push to your own Google Sheet:"),
        tags$ol(
          tags$li("Create a sheet and paste its URL above."),
          tags$li(
            "Share the sheet with ",
            tags$code(google_service_account_email()),
            " as Editor (you can uncheck \"Notify people\")."
          ),
          tags$li("Click ", tags$strong("Push Results to Google Sheet"), ".")
        ),
        "Leave the URL blank to use the default sheet. ",
        "Results are written to the ", tags$strong("DDC Results"), " tab."
      ),
      verbatimTextOutput("sheet_push_status")
    ),
    mainPanel(
      h4("Schedule, Courts, Byes, & Scoring"),
      uiOutput("manual_game_ui"),
      uiOutput("games_ui"),
      hr(),
      h4("Player Results"),
      uiOutput("shootout_resolution_ui"),
      DTOutput("player_table")
    )
  )
)

# ---------- SERVER ------------------------------------------------------------

server <- function(input, output, session) {
  session$allowReconnect(TRUE)  # allow automatic reconnect
  schedule_version <- reactiveVal(0)
  admin_unlocked <- reactiveVal(FALSE)
  finals_schedule_value <- reactiveVal(NULL)
  manual_games <- reactiveVal(empty_manual_games())

  observe({
    controls <- c("push_sheets")
    purrr::walk(controls, ~ {
      if (admin_unlocked()) shinyjs::enable(.x) else shinyjs::disable(.x)
    })
  })

  observeEvent(input$unlock_admin, {
    expected <- Sys.getenv("SCOREKEEPER_PASSWORD", "")
    if (!nzchar(expected)) {
      showNotification("SCOREKEEPER_PASSWORD is not configured.", type = "error", duration = 8)
      return()
    }

    if (identical(input$admin_password, expected)) {
      admin_unlocked(TRUE)
      shinyjs::disable("admin_password")
      shinyjs::disable("unlock_admin")
      showNotification("Scorekeeping unlocked.", type = "message", duration = 5)
    } else {
      showNotification("Incorrect admin password.", type = "error", duration = 5)
    }
  }, ignoreInit = TRUE)

  output$admin_status <- renderUI({
    if (admin_unlocked()) {
      tags$p(class = "text-success", tags$strong("Scorekeeping unlocked."))
    } else {
      tags$p(class = "text-muted", "Schedule setup is open. Scorekeeping is locked.")
    }
  })

  output$check_results_ui <- renderUI({
    sheet_url <- tryCatch(google_sheet_url(input$sheet_url), error = function(e) NULL)
    if (is.null(sheet_url)) {
      return(tagList(
        tags$button("Check Results", class = "btn btn-default", disabled = NA),
        helpText("Configure a Google Sheet to share results.")
      ))
    }

    tagList(
      tags$a(
        "Check Results",
        href = sheet_url,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "btn btn-default",
        role = "button"
      ),
      helpText("Opens the shared results sheet in a separate browser tab.")
    )
  })
  
  players_vec <- reactive({
    req(input$players_raw)
    x <- str_split(input$players_raw, "\\r?\\n")[[1]] |> str_trim()
    x <- x[nzchar(x)]
    shiny::validate(shiny::need(length(x) >= 4, "Please enter at least 4 players (up to 11 supported)."))
    shiny::validate(shiny::need(length(x) <= 11, "Please limit to 11 players for this version."))
    x
  })

  observeEvent(input$make_schedule, {
    schedule_version(schedule_version() + 1)
    finals_schedule_value(NULL)
    manual_games(empty_manual_games())
  }, ignoreInit = TRUE, priority = 100)
  
  prelim_schedule <- eventReactive(input$make_schedule, {
    players <- players_vec()
    mode <- input$format_mode %||% "single"

    if (mode == "two_group") {
      shiny::validate(shiny::need(length(players) >= 8, "Two-group format requires at least 8 players."))
      make_two_group_prelim_schedule(players)
    } else {
      add_schedule_context(make_schedule(players), phase = "Main", group = "All")
    }
  }, ignoreInit = TRUE)

  prelim_scores_tbl <- reactive({
    req(prelim_schedule())
    scores_for_games(prelim_schedule()$games, input, schedule_version())
  })

  preliminary_results <- reactive({
    req(prelim_schedule())
    if ((input$format_mode %||% "single") != "two_group") return(NULL)
    compute_two_group_results(prelim_schedule(), prelim_scores_tbl(), NULL, prelim_scores_tbl())
  })

  prelim_shootout_groups <- reactive({
    results <- preliminary_results()
    if (is.null(results) || !games_complete(prelim_scores_tbl())) return(list())
    find_shootout_groups(results)
  })

  resolved_preliminary_results <- reactive({
    results <- preliminary_results()
    if (is.null(results)) return(NULL)
    apply_shootout_resolutions(results, prelim_shootout_groups(), input)
  })

  schedule <- reactive({
    req(prelim_schedule())
    finals <- finals_schedule_value()
    games <- prelim_schedule()$games
    byes <- prelim_schedule()$byes

    if (!is.null(finals)) {
      games <- bind_rows(games, finals$games)
      byes <- bind_byes(byes, finals$byes)
    }

    if (nrow(manual_games()) > 0) {
      games <- bind_rows(games, manual_games())
    }

    list(
      games = games %>%
        arrange(factor(phase, levels = c("Main", "Preliminary", "Finals", "Manual")), group, round, court, game_id),
      byes = byes,
      groups = prelim_schedule()$groups
    )
  })

  # Collect scores directly from inputs (updates instantly as you type)
  scores_tbl <- reactive({
    req(schedule())
    scores_for_games(schedule()$games, input, schedule_version())
  })

  output$finals_controls <- renderUI({
    if (!admin_unlocked() || (input$format_mode %||% "single") != "two_group") return(NULL)
    if (is.null(prelim_schedule())) return(NULL)
    if (!games_complete(prelim_scores_tbl())) {
      return(tags$p(class = "text-muted", "Finish preliminary scores before generating finals."))
    }
    unresolved <- unresolved_shootout_labels(prelim_shootout_groups(), input)
    if (length(unresolved) > 0) {
      return(tags$p(class = "text-warning", "Resolve preliminary shootouts before generating finals."))
    }
    if (!is.null(finals_schedule_value())) {
      return(tags$p(class = "text-success", "Finals schedule generated."))
    }
    tagList(
      br(),
      actionButton("generate_finals", "Generate Finals", class = "btn-info")
    )
  })

  observeEvent(input$generate_finals, {
    req(admin_unlocked())
    req((input$format_mode %||% "single") == "two_group")
    req(games_complete(prelim_scores_tbl()))
    req(length(unresolved_shootout_labels(prelim_shootout_groups(), input)) == 0)
    finals_schedule_value(
      make_two_group_finals_schedule(
        prelim_schedule(),
        prelim_scores_tbl(),
        resolved_preliminary_results()
      )
    )
  }, ignoreInit = TRUE)

  output$manual_game_ui <- renderUI({
    if (!admin_unlocked() || is.null(prelim_schedule())) return(NULL)
    player_choices <- players_vec()

    tagList(
      hr(),
      h4("Add Game"),
      selectInput("manual_a1", "Team A player 1", choices = c("", player_choices)),
      selectInput("manual_a2", "Team A player 2", choices = c("", player_choices)),
      selectInput("manual_b1", "Team B player 1", choices = c("", player_choices)),
      selectInput("manual_b2", "Team B player 2", choices = c("", player_choices)),
      checkboxInput("manual_counts", "Counts in standings", value = TRUE),
      actionButton("add_manual_game", "Add Game", class = "btn-info")
    )
  })

  observeEvent(input$add_manual_game, {
    req(admin_unlocked())
    req(schedule())

    selected <- c(input$manual_a1, input$manual_a2, input$manual_b1, input$manual_b2)
    selected <- selected[nzchar(selected)]
    if (length(selected) != 4 || length(unique(selected)) != 4) {
      showNotification("Select four distinct players for the added game.", type = "error", duration = 6)
      return()
    }

    existing_ids <- c(schedule()$games$game_id, manual_games()$game_id)
    next_id <- if (length(existing_ids) == 0) 1L else max(existing_ids, na.rm = TRUE) + 1L
    next_round <- max(schedule()$games$round, na.rm = TRUE) + 1L

    manual_games(bind_rows(
      manual_games(),
      make_manual_game(
        game_id = next_id,
        round_number = next_round,
        team_a = selected[1:2],
        team_b = selected[3:4],
        count_in_standings = isTRUE(input$manual_counts)
      )
    ))

    showNotification("Added manual game.", type = "message", duration = 4)
  }, ignoreInit = TRUE)
  
  # AUTO-SAVE state to LocalStorage whenever players/scores/schedule change
  observe({
    if (is.null(schedule())) return()
    st <- list(
      players_raw = if (!is.null(input$players_raw)) input$players_raw else "",
      format_mode = input$format_mode %||% "single",
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

    if (!is.null(dat$format_mode) && dat$format_mode %in% c("single", "two_group")) {
      updateRadioButtons(session, "format_mode", selected = dat$format_mode)
    }
    
    # Keep restored players/format only. Scores start blank on each generated schedule.
  }, ignoreInit = FALSE)
  
  # Build the schedule UI (with byes under Team B)
  output$games_ui <- renderUI({
    req(schedule())
    sch  <- schedule()$games
    byes <- schedule()$byes  # tibble(round, byes) or NULL
    elems <- list()
    score_value <- function(prefix, gid) {
      value <- isolate(input[[score_input_id(prefix, gid, schedule_version())]])
      if (is.null(value)) NA else value
    }
    score_input <- function(prefix, gid) {
      input_control <- numericInput(
        score_input_id(prefix, gid, schedule_version()),
        prefix,
        value = score_value(prefix, gid),
        min = 0,
        step = 1
      )
      if (admin_unlocked()) input_control else shinyjs::disabled(input_control)
    }

    sections <- sch %>%
      distinct(phase, group) %>%
      arrange(factor(phase, levels = c("Main", "Preliminary", "Finals")), group)

    for (section_idx in seq_len(nrow(sections))) {
      phase_name <- sections$phase[section_idx]
      group_name <- sections$group[section_idx]
      section_games <- sch %>%
        filter(phase == phase_name, group == group_name) %>%
        arrange(round, court, game_id)

      if (!(phase_name == "Main" && group_name == "All")) {
        elems <- append(elems, list(h4(sprintf("%s - %s", phase_name, group_name))))
      }

      for (r in sort(unique(section_games$round))) {
        sub <- section_games %>% dplyr::filter(round == r) %>% dplyr::arrange(court)
        elems <- append(elems, list(div(class="round-header", strong(sprintf("Round %d", r)))))

        round_byes <- NULL
        if (!is.null(byes)) {
          b <- byes %>% dplyr::filter(phase == phase_name, group == group_name, round == r)
          if (nrow(b) == 1 && !is.na(b$byes) && nzchar(b$byes)) round_byes <- paste0("Byes: ", b$byes)
        }

        for (i in seq_len(nrow(sub))) {
          gid <- sub$game_id[i]
          score_column <- if (admin_unlocked()) {
            column(
              5,
              div(class = "score-box small-input",
                  score_input("A", gid)
              ),
              div(class = "score-box small-input",
                  score_input("B", gid)
              )
            )
          } else {
            NULL
          }

          elems <- append(elems, list(
            fluidRow(
              column(
                if (admin_unlocked()) 7 else 12,
                strong(sprintf("Game %d (Court %d):", gid, sub$court[i])),
                div(sprintf("Team A: %s", sub$teamA_display[i])),
                div(sprintf("Team B: %s", sub$teamB_display[i])),
                if (!isTRUE(sub$count_in_standings[i])) div(class="bye-line", "Does not count in standings"),
                if (!is.null(round_byes)) div(class="bye-line", round_byes)
              ),
              score_column
            ),
            hr()
          ))
        }
      }
    }
    do.call(tagList, elems)
  })
  
  player_results <- reactive({
    req(schedule())
    if ((input$format_mode %||% "single") == "two_group") {
      return(compute_two_group_results(prelim_schedule(), prelim_scores_tbl(), finals_schedule_value(), scores_tbl()))
    }
    counted_games <- schedule()$games %>% filter(count_in_standings)
    counted_scores <- scores_tbl() %>% semi_join(counted_games, by = "game_id")
    compute_player_results(players_in_games(counted_games, players_vec()), counted_games, counted_scores)
  })

  current_shootout_groups <- reactive({
    req(player_results())
    if ((input$format_mode %||% "single") == "two_group") {
      prelim_groups <- prelim_shootout_groups()
      if (length(unresolved_shootout_labels(prelim_groups, input)) > 0) return(prelim_groups)

      finals <- finals_schedule_value()
      if (is.null(finals)) return(list())
      finals_scores <- scores_tbl() %>% semi_join(finals$games, by = "game_id")
      if (!games_complete(finals_scores)) return(list())
    } else if (!games_complete(scores_tbl())) {
      return(list())
    }

    find_shootout_groups(player_results())
  })

  output$shootout_resolution_ui <- renderUI({
    if (!admin_unlocked()) return(NULL)
    groups <- current_shootout_groups()
    if (length(groups) == 0) return(NULL)

    tagList(
      div(
        class = "well",
        h4("Shootout Resolution"),
        purrr::map(groups, function(group_info) {
          players <- unlist(group_info$players)
          seeds <- unlist(group_info$seeds)
          players <- players[order(seeds)]
          seeds <- sort(seeds)

          if (length(players) == 3) {
            top_seed <- players[1]
            lower_seeds <- players[2:3]
            first_winner <- input[[paste0(group_info$id, "_first_winner")]] %||% ""
            final_choices <- if (nzchar(first_winner)) c(top_seed, first_winner) else top_seed

            return(tagList(
              tags$strong(group_info$label),
              div(
                class = "bye-line",
                sprintf(
                  "First shootout: %s vs %s. Winner advances to play %s.",
                  lower_seeds[1],
                  lower_seeds[2],
                  top_seed
                )
              ),
              fluidRow(
                column(
                  6,
                  selectInput(
                    paste0(group_info$id, "_first_winner"),
                    "First shootout winner",
                    choices = c("", lower_seeds),
                    selected = first_winner
                  )
                ),
                column(
                  6,
                  selectInput(
                    paste0(group_info$id, "_final_winner"),
                    "Second shootout winner",
                    choices = c("", final_choices),
                    selected = input[[paste0(group_info$id, "_final_winner")]] %||% ""
                  )
                )
              )
            ))
          }

          tagList(
            tags$strong(group_info$label),
            fluidRow(
              column(
                6,
                selectInput(
                  paste0(group_info$id, "_winner"),
                  "Winner",
                  choices = c("", players),
                  selected = input[[paste0(group_info$id, "_winner")]] %||% ""
                )
              ),
              column(
                6,
                selectInput(
                  paste0(group_info$id, "_loser"),
                  "Loser",
                  choices = c("", players),
                  selected = input[[paste0(group_info$id, "_loser")]] %||% ""
                )
              )
            )
          )
        })
      )
    )
  })

  output$player_table <- renderDT({
    req(player_results())
    tbl <- player_results()
    hidden_targets <- which(names(tbl) == "Seed") - 1
    datatable(
      tbl,
      rownames = FALSE,
      options = list(
        dom = "tip",
        pageLength = 25,
        columnDefs = list(list(visible = FALSE, targets = hidden_targets))
      )
    ) %>%
      formatPercentage("Win %", digits = 1) %>%
      formatStyle(
        "Rank",
        fontWeight = styleEqual("Shootout", "bold"),
        color = styleEqual("Shootout", "#b45309")
      )
  })

  output$sheet_push_status <- renderText({
    sheet_push_status()
  })

  sheet_push_status <- reactiveVal("")

  observeEvent(input$push_sheets, {
    req(admin_unlocked())
    sheet_push_status("Pushing results to Google Sheet...")
    result <- tryCatch({
      shootout_groups <- current_shootout_groups()
      unresolved <- unresolved_shootout_labels(shootout_groups, input)
      if (length(unresolved) > 0) {
        stop(
          paste0(
            "Resolve the shootout before pushing: ",
            paste(unresolved, collapse = "; ")
          ),
          call. = FALSE
        )
      }

      tbl <- apply_shootout_resolutions(player_results(), shootout_groups, input)
      req(nrow(tbl) > 0)
      ss_id <- extract_sheet_id(input$sheet_url)
      tabs <- list("DDC Results" = tbl)

      if ((input$format_mode %||% "single") == "two_group") {
        prelim_groups <- prelim_shootout_groups()
        prelim_unresolved <- unresolved_shootout_labels(prelim_groups, input)
        if (length(prelim_unresolved) > 0) {
          stop(
            paste0(
              "Resolve the preliminary shootout before pushing: ",
              paste(prelim_unresolved, collapse = "; ")
            ),
            call. = FALSE
          )
        }

        prelim_tbl <- apply_shootout_resolutions(resolved_preliminary_results(), prelim_groups, input)
        tabs[["Prelim Results"]] <- prelim_tbl
      }

      sheet_url <- push_result_tabs_to_sheet(tabs, ss_id)
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
