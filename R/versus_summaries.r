#' For a set of possible decks, order by probability of being best counter to given deck
#'
#' @param deck A character, giving the name of a single deck to counter.
#' @param vs_array The versus model array to use, as returned by
#'   \code{\link{extract_vs_model_array}}.
#' @param type List of decks to use. \code{multi} uses all legal multicolour decks.
#'   \code{draft} also includes decks where none of the specs are from the same
#'   colour as the starter. \code{mono} only includes monocolour decks.
#'
#' @return A data.table with three columns: \code{deck} for the name of the
#'   countering deck, \code{prob_best} for the probability that it's the best counter
#'   deck, and \code{mean_win_prob} for its mean win probability against the deck.
#' @export
counter <- function(deck, vs_array, type = c("multi", "draft", "mono")) {
  type <- match.arg(type)
  components <- unlist(codexdata::components(deck, codexdata::starters, codexdata::nicknames))
  both_array <- vs_array - aperm(vs_array, c(1, 3, 2))
  counter_array <- apply(both_array[, , components], c(1, 2), sum)
  counter_component_sets <- switch(type,
                                   "multi" = codexdata:::multicolour_deck_components,
                                   "draft" = codexdata:::draft_deck_components,
                                   "mono" = codexdata:::monocolour_deck_components)
  counter_deck_names <- switch(type,
                               "multi" = codexdata:::multicolour_deck_names,
                               "draft" = codexdata:::draft_deck_names,
                               "mono" = codexdata:::monocolour_deck_names)
  possible_counter_samples <- apply(counter_component_sets, 1,
                                    function(comps) rowSums(counter_array[, unlist(comps)]))
  dimnames(possible_counter_samples) <- list(NULL, counter_deck_names)
  best_array <- apply(possible_counter_samples, 1, function(x) x == max(x))
  best_prob <- sort(rowMeans(best_array), decreasing = TRUE)
  mean_win_probs <- colMeans(1/(1 + exp(-possible_counter_samples[, names(best_prob)])))
  data.table(Deck = names(best_prob), `Probability best counter` = unname(best_prob),
             `Counter win probability` = mean_win_probs)
}

#' Extract versus model opposed component pair effect array
#'
#' @inheritParams get_matchup_array
#'
#' @return An array of opposed component pair effects, with dimensions equal to
#'   number of samples, then the number of starters plus numbers of specs twice.
#' @export
extract_vs_model_array <- function(
  tidy_results
) {
  St <- tidy_results$model_data$St
  Sp <- tidy_results$model_data$Sp
  starters <- tidy_results$model_data$starters
  specs <- tidy_results$model_data$specs
  arr <- array(dim = c(nrow(tidy_results$tidy_results$starter_vs_starter),
                       St + Sp,
                       St + Sp))
  for (i in 1:dim(arr)[1]) {
    arr[i, , ] <- rbind(cbind(tidy_results$tidy_results$starter_vs_starter[i, , ],
                              tidy_results$tidy_results$starter_vs_spec[i, , ]),
                        cbind(tidy_results$tidy_results$spec_vs_starter[i, , ],
                              tidy_results$tidy_results$spec_vs_spec[i, , ]))
  }
  dimnames(arr) <- list(NULL, c(starters, specs), c(starters, specs))
  arr
}

#' Extract versus model component variance sample information
#'
#' @inheritParams get_matchup_array
#'
#' @return A data.table.
#' @export
get_variances <- function(
  tidy_results
) {
  base <- data.table(
    `player skill` = 2 * as.numeric(sim$sd_player)^2,
    `starter vs. starter` = as.numeric(sim$sd_starter_vs_starter)^2,
    `starter vs. spec` = 3 * as.numeric(sim$sd_starter_vs_spec)^2,
    `spec vs. spec` = 9 * as.numeric(sim$sd_spec_vs_spec)^2
  )
  base[,
       c(.SD,
         list(`total deck` =
                `starter vs. starter` +
                `starter vs. spec` +
                `spec vs. spec`))
       ][,
         c(.SD,
           list(`player/deck ratio` =
                  `player skill`/`total deck`))] %>%
    melt(id.vars = character(0), variable.name = "type")
}

#' Plot versus model variance sample information
#'
#' @param variances A data.table of variances, as returned by
#'   \code{\link{get_variances}}.
#'
#' @return A ggplot object.
#' @export
plot_variances <- function(variances) {
  ggplot2::ggplot(variances, ggplot2::aes(x = .data$type, y = .data$value)) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle("Component variances, scaled by number of instances") +
    ggplot2::xlab("type") +
    ggplot2::ylab("variance")
}

#' Create an array of matchup matrix samples for given decks/players
#'
#' @param tidy_results A list, containing a versus model's tidy results.
#' @param deck_names A character vector, giving standardised deck names to get
#'   matchups for. Standardised names may used the package's default nicknames.
#' @param players A character vector, giving players to assign to the given
#'   decks, in order. If NULL, players are not accounted for. Otherwise, the
#'   length must be equal to the length of the deck names. Players not present
#'   in the model information will have their skill samples generated.
#' @param player_seed An integer, giving the random seed used to generate
#'   unknown players' skill samples.
#'
#' @return An array, with dimensions (sample, player 1, player 2).
#' @export
get_matchup_array <- function(
  tidy_results,
  deck_names,
  players = NULL,
  player_seed = 1
) {
  if (!is.null(players) && length(players) != length(deck_names))
    stop("deck_names and players lengths must match")
  if (is.null(players))
    deck_names <- unique(deck_names)
  player_info <- get_player_skills(
    tidy_results$tidy_results,
    players,
    player_seed
  )
  deck_components <- codexdata::components(
    deck_names,
    codexdata::starters,
    codexdata::nicknames
  )
  deck_info <- get_deck_component_matchups(
    tidy_results,
    deck_components,
    player_seed
  )
  deck_effects <-
    deck_info[, deck_components$starter, deck_components$starter] +
    deck_info[, deck_components$starter, deck_components$spec1] +
    deck_info[, deck_components$starter, deck_components$spec2] +
    deck_info[, deck_components$starter, deck_components$spec3] +
    deck_info[, deck_components$spec1, deck_components$starter] +
    deck_info[, deck_components$spec1, deck_components$spec1] +
    deck_info[, deck_components$spec1, deck_components$spec2] +
    deck_info[, deck_components$spec1, deck_components$spec3] +
    deck_info[, deck_components$spec2, deck_components$starter] +
    deck_info[, deck_components$spec2, deck_components$spec1] +
    deck_info[, deck_components$spec2, deck_components$spec2] +
    deck_info[, deck_components$spec2, deck_components$spec3] +
    deck_info[, deck_components$spec3, deck_components$starter] +
    deck_info[, deck_components$spec3, deck_components$spec1] +
    deck_info[, deck_components$spec3, deck_components$spec2] +
    deck_info[, deck_components$spec3, deck_components$spec3]
  dimnames(deck_effects) <- list(NULL, deck_names, deck_names)
  if (is.null(players))
    return(plogis(deck_effects))
  repeated_player_info <- apply(player_info[, players], 2, rep, length(players))
  P1_effect <- aperm(
    array(
      repeated_player_info,
      dim = c(nrow(player_info), length(players), length(players)),
      dimnames = list(iteration = NULL, P2 = players, P1 = players)
    ),
    c(1, 3, 2)
  )
  P2_effect <- aperm(P1_effect, c(1, 3, 2))
  player_effects <- P1_effect - P2_effect
  total_effects <- deck_effects + player_effects
  dimnames(total_effects) <- list(
    iteration = NULL,
    paste(players, deck_names, sep = ":"),
    paste(players, deck_names, sep = ":")
  )
  plogis(total_effects)
}

#' Get player skill sample array for given players, including unknown players
#'
#' @inheritParams get_matchup_array
#'
#' @return A matrix of log-odds player effects on P1 victory when the player is
#'   P1, with dimensions (sample, player).
#' @export
get_player_skills <- function(
  tidy_results,
  players,
  player_seed = 1
) {
  player_info <- tidy_results$player
  if (all(is.element(players, colnames(player_info))))
    return(player_info)
  set.seed(player_seed)
  missing <- setdiff(players, colnames(player_info))
  sd_players <- sim$sd_player
  added <- matrix(
    stats::rnorm(length(missing) * nrow(player_info), sd = drop(sd_players)),
    ncol = length(missing),
    dimnames = list(NULL, missing)
  )
  cbind(player_info, added)
}

get_deck_component_matchups <- function(
  vs_results,
  deck_components,
  deck_seed = 1
) {
  deck_info <- if (is.element("vs_array", names(vs_results)))
    vs_results$vs_array
  else
    extract_vs_model_array(vs_results)
  missing_starters <- setdiff(deck_components$starter, colnames(deck_info))
  missing_specs <- setdiff(
    c(deck_components$spec1, deck_components$spec2, deck_components$spec3),
    colnames(deck_info)
  )
  if (length(missing_starters) == 0 && length(missing_specs) == 0)
    return(deck_info)
  nonmissing_starters <- intersect(deck_components$starter, colnames(deck_info))
  all_starters <- c(nonmissing_starters, missing_starters)
  nonmissing_specs <- intersect(
    c(deck_components$spec1, deck_components$spec2, deck_components$spec3),
    colnames(deck_info)
  )
  all_specs <- c(nonmissing_specs, missing_specs)
  n_samples <- nrow(deck_info)

  new_deck_info <- array(
    dim = c(
      n_samples,
      length(all_starters) + length(all_specs),
      length(all_starters) + length(all_specs)
    ),
    dimnames = list(
      NULL,
      c(all_starters, all_specs),
      c(all_starters, all_specs)
    )
  )
  new_deck_info[
    ,
    c(nonmissing_starters, nonmissing_specs),
    c(nonmissing_starters, nonmissing_specs)
  ] <- deck_info[
    ,
    c(nonmissing_starters, nonmissing_specs),
    c(nonmissing_starters, nonmissing_specs)
  ]

  sd_StSt <- if (is.element("sd_starter_vs_starter", names(vs_results$tidy_results)))
    vs_results$sd_starter_vs_starter
  else
    sqrt(vs_results$tidy_results$var_starter_vs_starter)
  StSt_missing <- is.na(new_deck_info[, all_starters, all_starters])
  new_deck_info[
    , all_starters, all_starters
  ][
    StSt_missing
  ] <- rnorm(sum(StSt_missing), sd = drop(sd_StSt))

  sd_StSp <- if (is.element("sd_starter_vs_spec", names(vs_results$tidy_results)))
    vs_results$tidy_results$sd_starter_vs_spec
  else
    sqrt(vs_results$tidy_results$var_starter_vs_spec)
  StSp_missing <- is.na(new_deck_info[, all_starters, all_specs])
  SpSt_missing <- is.na(new_deck_info[, all_specs, all_starters])
  new_deck_info[
    , all_starters, all_specs
  ][
    StSp_missing
  ] <- rnorm(sum(StSp_missing), sd = drop(sd_StSp))
  new_deck_info[
    , all_specs, all_starters
  ][
    SpSt_missing
  ] <- rnorm(sum(SpSt_missing), sd = drop(sd_StSp))

  sd_SpSp <- if (is.element("sd_spec_vs_spec", names(vs_results$tidy_results)))
    vs_results$tidy_results$sd_spec_vs_spec
  else
    sqrt(vs_results$tidy_results$var_spec_vs_spec)
  SpSp_missing <- is.na(new_deck_info[, all_specs, all_specs])
  new_deck_info[
    , all_specs, all_specs
  ][
    SpSp_missing
  ] <- rnorm(sum(SpSp_missing), sd = drop(sd_SpSp))

  stopifnot(all(
    deck_info[
      ,
      c(nonmissing_starters, nonmissing_specs),
      c(nonmissing_starters, nonmissing_specs)
    ] ==
      new_deck_info[
        ,
        c(nonmissing_starters, nonmissing_specs),
        c(nonmissing_starters, nonmissing_specs)
      ]
  ))
  new_deck_info
}

#' Get sample data.table for matchup samples array
#'
#' @param matchup_array An array, giving samples of matchup matrix information.
#'
#' @return A data.table with four columns: \code{P1} and \code{P2} for the
#'   decks/players involved, \code{prob_matchup} for the sampled P1 win
#'   probability, and \code{prob_mean} for the mean P1 win probability over all
#'   samples for that matchup, pre-calculated for use in
#'   \code{\link{plot_matchup_samples}}.
#' @export
get_matchups_from_array <- function(matchup_array){
  names <- dimnames(matchup_array)[[2]]
  melt(as.data.table(as.data.frame(matchup_array)),
       measure.vars = as.vector(outer(names, names, paste, sep = ".")),
       variable.name = "decks", value.name = "matchup"
  )[, c(lapply(tstrsplit(decks, "\\.", names = c("P1", "P2")), factor, unique(names)),
        list(prob_matchup = matchup))
    ][, c(.SD, list(prob_mean = mean(prob_matchup))),
      by = c("P1", "P2")]
}

#' Get matchup samples data.table for given decks/players
#'
#' @inheritParams get_matchup_array
#'
#' @return A data.table with four columns: \code{P1} and \code{P2} for the
#'   decks/players involved, \code{prob_matchup} for the sampled P1 win
#'   probability, and \code{prob_mean} for the mean P1 win probability over all
#'   samples for that matchup, pre-calculated for use in
#'   \code{\link{plot_matchup_samples}}.
#' @export
get_matchups <- function(
  tidy_results,
  deck_names,
  players = NULL,
  player_seed = 1
) {
  get_matchups_from_array(get_matchup_array(tidy_results, deck_names, players, player_seed))
}

#' Create an HTML table widget for a mean matchup matrix
#'
#' @param mean_matchup A matrix, containing a single-sample matchup matrix.
#' @param prefix A character, giving the prefix to use for the two deck/player
#'   columns.
#' @param ... Other arguments to datatable.
#'
#' @return A datatable.
#' @export
DT_mean_matchup <- function(mean_matchup, prefix, ...) {
  dt <- reformat_mean_matchup(mean_matchup, prefix)
  DT::datatable(dt,
                filter = "top", ...) %>%
    DT::formatRound(3:4, 3)
}

#' Reformat mean matchup array into tall data.table format
#'
#' @inheritParams DT_mean_matchup
#'
#' @return A data.table.
#' @export
reformat_mean_matchup <- function(mean_matchup, prefix) {
  pre1 <- paste0(prefix, "1")
  pre2 <- paste0(prefix, "2")
  melt(as.data.table(mean_matchup, keep.rownames = "P1")[, c(list(P1 = factor(P1, unique(P1))), .SD),
                                                         .SDcols = setdiff(colnames(mean_matchup),
                                                                           "P1")],
       id.vars = "P1", variable.name = "P2",
       value.name = "matchup")[P1 != P2
                               ][, c(stats::setNames(.SD, c(pre1, pre2, "matchup")),
                                     list(fairness = 1 - 2*abs(matchup - 1/2)))
                                 ][order(-fairness)]
}
