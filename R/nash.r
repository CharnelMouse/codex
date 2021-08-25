#' Get Nash equilibirum information for matchup matrix / samples array
#'
#' @param matchups Either a matrix or an array, containing a Nash equilibirum
#'   information.
#'
#' @return Either a matrix or an array of Nash equilibria, depending on the
#'   given input's class.
#' @export
get_nash_equilibria <- function(matchups) {
  UseMethod("get_nash_equilibria")
}

#' @export
get_nash_equilibria.matrix <- function(matchups) {
  deck_names <- rownames(matchups)
  if (!identical(deck_names, colnames(matchups)))
    stop("matrix row/column names don't match")
  name_dup <- which(!duplicated(deck_names))
  matchups <- as.matrix(matchups[name_dup, name_dup])
  deck_names <- deck_names[name_dup]

  n_decks <- nrow(matchups)
  P1_mat <- t(matchups)
  P1_solution <- lpSolve::lp(direction = "min", rep(1, n_decks), rbind(diag(n_decks), P1_mat),
                             rep(">=", 2*n_decks), rep(0:1, each = n_decks))
  P1_value <- 1/sum(P1_solution$solution)
  P2_mat <- 1 - matchups
  P2_solution <- lpSolve::lp(direction = "min", rep(1, n_decks), rbind(diag(n_decks), P2_mat),
                             rep(">=", 2*n_decks), rep(0:1, each = n_decks))
  P2_value <- 1/sum(P2_solution$solution)
  both_solution <- lpSolve::lp(direction = "min", rep(1, n_decks),
                               rbind(diag(n_decks), (P1_mat + P2_mat)/2),
                               rep(">=", 2*n_decks), rep(0:1, each = n_decks))
  both_value <- 1/sum(both_solution$solution)
  cbind(data.table(Player = factor(c("P1", "P2", "Both"), c("P1", "P2", "Both")),
                   `Win probability` = c(P1_value, 1 - P1_value, both_value)),
        rbind(stats::setNames(P1_solution$solution*P1_value, deck_names),
              stats::setNames(P2_solution$solution*P2_value, deck_names),
              stats::setNames(both_solution$solution*both_value, deck_names)))
}

#' @export
get_nash_equilibria.array <- function(matchups) {
  vapply(1:dim(matchups)[[1]],
         function(int) as.matrix(get_nash_equilibria.matrix(matrix(matchups[int, , ],
                                                                   nrow = dim(matchups)[2],
                                                                   dimnames = dimnames(matchups)[2:3])),
                                 rownames = "Player"),
         matrix(0, nrow = 3, ncol = 1 + length(unique(dimnames(matchups)[[3]]))))
}

#' Get mean matchup array directly from sim
#'
#' This is similar to calling \code{\link{get_matchup_array}}, then averaging
#' over the samples. The difference is that the matchup samples are calculated
#' one at a time. This allows calculating the mean matchup in cases where the
#' matchup array cannot fit into memory.
#'
#' @inheritParams get_matchup_array
#' @param deck_set List of decks to use. \code{multi} uses all legal multicolour decks.
#'   \code{draft} also includes decks where none of the specs are from the same
#'   colour as the starter. \code{mono} only includes monocolour decks.
#'   \code{custom} allows a custom set of decks, given in the \code{deck_names}
#'   argument, that is otherwise ignored.
#' @param progress If TRUE, and parallel = FALSE, messages the current sample
#'   number.
#'
#' @return A mean matchup matrix.
#' @export
get_mean_matchup_array_from_sim <- function(sim, deck_set = c("multi", "draft", "mono", "custom"),
                                            deck_names = stop("deck_names must be specified for custom sets"),
                                            players = NULL, player_seed = 1, progress = FALSE) {
  with(get_info_for_nash_sims(sim, deck_set, deck_names, players, player_seed), {
    res <- matrix(NA_real_, ncol = length(deck_names), nrow = length(deck_names))
    # paralellize using foreach's .combine argument?
    for (n in seq.int(n_iter)) {
      if (progress)
        message(paste(n, "of", n_iter))
      matchups_sample <- get_matchups_sample(n, deck_info, deck_names, deck_components, player_info, players)
      if (n == 1L) {
        res <- matchups_sample
      }
      else
        res <- res + matchups_sample
    }
    res/n_iter
  })
}

#' Get Nash sample array directly from sim
#'
#' This gets the sample array of Nash equilibria without loading the whole
#' matchup array into memeory as an intermediate step. This allows calculating
#' the Nash equilibrium in cases where the matchup array cannot fit into memory.
#' This can also be run in parallel.
#'
#' @inheritParams get_mean_matchup_array_from_sim
#' @param parallel FALSE to run normally. To run in parallel, set to an integer
#'   equal to the number of processes.
#'
#' @return A Nash equilibrium array.
#' @export
get_nash_array_from_sim <- function(sim, deck_set = c("multi", "draft", "mono", "custom"),
                                    deck_names = stop("deck_names must be specified for custom sets"),
                                    players = NULL, player_seed = 1, progress = FALSE,
                                    parallel = FALSE) {
  with(get_info_for_nash_sims(sim, deck_set, deck_names, players, player_seed), {
    if (!parallel)
      vapply(seq.int(n_iter),
             function(n) {
               if (progress)
                 message(paste(n, "of", n_iter))
               matchups_sample <- get_matchups_sample(n, deck_info, deck_names, deck_components, player_info, players)
               get_nash_equilibria.array(array(matchups_sample,
                                               dim = c(1L, dim(matchups_sample)),
                                               dimnames = c(list(NULL), dimnames(matchups_sample))))
             },
             array(NA_real_,
                   dim = c(3L, 1L + if (is.null(players)) length(deck_names) else length(players)),
                   dimnames = list(c("P1", "P2", "Both"),
                                   c("Win probability", if (is.null(players)) deck_names else players))))
    else {
      cluster <- parallel::makeCluster(parallel)
      res <- parallel::parLapply(cl = cluster,
                                 seq.int(n_iter),
                                 function(n) {
                                   if (progress)
                                     message(paste(n, "of", n_iter))
                                   matchups_sample <- get_matchups_sample(n, deck_info, deck_names, deck_components, player_info, players)
                                   get_nash_equilibria.array(array(matchups_sample,
                                                                   dim = c(1L, dim(matchups_sample)),
                                                                   dimnames = c(list(NULL), dimnames(matchups_sample))))
                                 })
      parallel::stopCluster(cluster)
      array(do.call(c, res),
            dim = c(3L, 1L + if (is.null(players)) length(deck_names) else length(players), n_iter),
            dimnames = list(c("P1", "P2", "Both"),
                            c("Win probability", if (is.null(players)) deck_names else players),
                            NULL))
    }
  })
}

get_info_for_nash_sims <- function(sim,
                                   deck_set = c("multi", "draft", "mono", "custom"),
                                   deck_names = stop("deck_names must be specified for custom sets"),
                                   players = NULL,
                                   player_seed = 1) {
  match.arg(deck_set)
  deck_names <- switch(deck_set,
                       "multi" = codexdata:::multicolour_deck_names,
                       "draft" = codexdata:::draft_deck_names,
                       "mono" = codexdata:::monocolour_deck_names,
                       "custom" = deck_names)
  if (!is.null(players) && length(players) != length(deck_names))
    stop("deck_names and players lengths must match")
  if (is.null(players))
    deck_names <- unique(deck_names)
  deck_info <- if (is.element("vs_array", names(sim)))
    sim$vs_array
  else
    extract_vs_model_array(sim)
  list(deck_names = deck_names,
       player_info = get_player_skills(sim$tidy_results, players, player_seed),
       deck_components = switch(deck_set,
                                "multi" = codexdata:::multicolour_deck_components,
                                "draft" = codexdata:::draft_deck_components,
                                "mono" = codexdata:::monocolour_deck_components,
                                "custom" = codexdata::components(deck_names,
                                                                 codexdata::starters,
                                                                 codexdata::nicknames)),
       deck_info = deck_info,
       n_iter = dim(deck_info)[[1]])
}

get_matchups_sample <- function(n, deck_info, deck_names, deck_components, player_info, players = NULL) {
  deck_effects_sample <- deck_info[n, deck_components$starter, deck_components$starter] +
    deck_info[n, deck_components$starter, deck_components$spec1] +
    deck_info[n, deck_components$starter, deck_components$spec2] +
    deck_info[n, deck_components$starter, deck_components$spec3] +
    deck_info[n, deck_components$spec1, deck_components$starter] +
    deck_info[n, deck_components$spec1, deck_components$spec1] +
    deck_info[n, deck_components$spec1, deck_components$spec2] +
    deck_info[n, deck_components$spec1, deck_components$spec3] +
    deck_info[n, deck_components$spec2, deck_components$starter] +
    deck_info[n, deck_components$spec2, deck_components$spec1] +
    deck_info[n, deck_components$spec2, deck_components$spec2] +
    deck_info[n, deck_components$spec2, deck_components$spec3] +
    deck_info[n, deck_components$spec3, deck_components$starter] +
    deck_info[n, deck_components$spec3, deck_components$spec1] +
    deck_info[n, deck_components$spec3, deck_components$spec2] +
    deck_info[n, deck_components$spec3, deck_components$spec3]
  deck_effects_sample <- array(deck_effects_sample, dim = c(length(deck_names), length(deck_names)))
  dimnames(deck_effects_sample) <- list(deck_names, deck_names)
  if (is.null(players))
    stats::plogis(deck_effects_sample)
  else{
    repeated_player_info <- rep(player_info[n, players], length(players))
    P1_effect <- array(repeated_player_info,
                       dim = c(length(players), length(players)),
                       dimnames = list(P2 = players, P1 = players))
    P2_effect <- aperm(P1_effect, c(2, 1))
    player_effects_sample <- P1_effect - P2_effect
    total_effects_sample <- deck_effects_sample + player_effects_sample
    dimnames(total_effects_sample) <- list(players, players)
    stats::plogis(total_effects_sample)
  }
}

#' Get mean Nash equilibrium over sample array
#'
#' @param nash_array An array, containing Nash samples.
#'
#' @return A data.table, giving the mean Nash equilibrium.
#' @export
nash_mean <- function(nash_array) {
  as.data.table(apply(nash_array, 1:2, mean), keep.rownames = "Player")
}

#' Plot matchup sample data.table, using facets over match pairings
#'
#' @param matchup_samples A data.table, giving matchup sample information.
#' @param break_interval A number, giving the increment to use for the matchup axis.
#'
#' @return A ggplot object.
#' @export
plot_matchup_samples <- function(matchup_samples, break_interval = 1) {
  ggplot2::ggplot(matchup_samples, ggplot2::aes(x = "", y = .data$prob_matchup, fill = .data$prob_mean)) +
    ggplot2::geom_violin(draw_quantiles = c(1/4, 1/2, 3/4)) +
    ggplot2::coord_flip() +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("matchup") +
    ggplot2::scale_y_continuous(breaks = seq.int(0, 10, by = break_interval)/10,
                                labels = seq.int(0, 10, by = break_interval)) +
    ggplot2::facet_grid(P1 ~ P2) +
    ggplot2::scale_fill_viridis_c(option = "B", guide = "none")
}

#' Print win probability information for matchup Nash equilibrium
#'
#' @param nash A matrix, containing a single Nash equilibrium information table.
#'
#' @return A matrix, containing the win probability information for the Nash
#'   equilibrium.
#' @export
print_nash <- function(nash) {
  nash[, 1:2]
}

#' Create an HTML table widget for a Nash equilibrium data.table
#'
#' @inheritParams print_nash
#' @param round An integer, giving the number of significant digits to round
#'   deck weights to.
#' @param ... Arguments to pass to \code{\link[DT]{datatable}}.
#'
#' @return A datatable.
#' @export
DT_nash <- function(nash, round = 3, ...) {
  nash_used <- reformat_used_nash(nash)
  DT::datatable(nash_used,
                rownames = FALSE, filter = "top",
                ...) %>%
    DT::formatRound(2:4, digits = round)
}

#' Reformat Nash table to give results only for used strategies
#'
#' @inheritParams print_nash
#'
#' @return A data.table, with rows for each used strategy and columns for each
#'   game, sorted for "Both" game.
#' @export
reformat_used_nash <- function(nash) {
  nonzero_colnames <- c("Player", "Win probability",
                        names(which(apply(nash[, -(1:2)], 2, function(x) any(x != 0)))))
  shaped <- melt(nash,
                 id.vars = "Player", measure.vars = nonzero_colnames[-(1:2)],
                 variable.name = "Deck", value.name = "Matchup") %>%
    dcast(Deck ~ Player, value.var = "Matchup")
  shaped$Deck <- as.character(shaped$Deck)
  shaped[order(-Both, Deck)]
}
