#' Get tidy model results from match data
#'
#' @param results A stan object.
#' @param model_data A list, containing data used for the simulation in
#'   \code{results}.
#' @param matches A data.frame containing Codex match data.
#' @param array if TRUE, adds a \code{vs_array} element to the result,
#'   containing the versus model array.
#' @param log_lik if TRUE, extracts log_lik samples to use in cross-validation.
#'
#' @return A list containing model data, tidy results, and simulation
#'   diagnostics.
#' @export
get_tidy_model_results <- function(results, model_data, matches, array = FALSE, log_lik = FALSE) {
  divergences <- sum(rstan::get_divergent_iterations(results))
  treedepths <- sum(rstan::get_max_treedepth_iterations(results))
  bad_chains <- sum(rstan::get_bfmi(results) < 0.2)
  match_names <- match_names(matches)
  tidy_results <- tidy_results(results, match_names, model_data)
  sim <- list(diagnostics = c(divergences = divergences, max_treedepth_iterations = treedepths, bad_chains = bad_chains),
              model_data = model_data, tidy_results = tidy_results)
  c(sim,
    if (array)
      list(vs_array = extract_vs_model_array(sim)),
    if (log_lik)
      list(log_lik = loo::extract_log_lik(results, merge_chains = FALSE)))
}

match_names <- function(matches) {
  paste0(matches$tournament,
         " Round ", matches$round,
         ", Game ", matches$round_match_number,
         ": ", matches$player1, " ", matches$deck1,
         " vs. ",
         matches$player2, " ", matches$deck2,
         ", won by ", matches$victor)
}

#' Prepare match data for input to Stan model
#'
#' @param matches A data.frame containing Codex match data.
#' @param starters A data.frame containing starter/spec pairs.
#' @param nicknames A data.frame containing deck name/nickname pairs.
#' @param mean A boolean. If FALSE, prepares data for a simple non-opposed
#'   model, i.e. where deck strength is independent of the opposing deck. If
#'   TRUE, prepares data for an opposed model
#' @return A list to pass to \code{stan()}'s \code{data} argument.
#' @export
prepare_match_data_for_modelling <- function(matches, starters, nicknames, mean = FALSE) {
  players <- sort(unique(c(matches$player1, matches$player2)))
  deck_names <- sort(unique(c(matches$deck1, matches$deck2)))
  base <- list(M = nrow(matches),
               P = length(players),
               D = length(deck_names),
               first_player = match(matches$player1, players),
               second_player = match(matches$player2, players),
               first_deck = match(matches$deck1, deck_names),
               second_deck = match(matches$deck2, deck_names),
               w = ifelse(matches$victor == matches$player1, 1L, 0L),
               players = players,
               deck_names = deck_names)
  if (!mean) return(base)
  deck_details1 <- codexdata::components(matches$deck1, starters, nicknames)
  deck_details2 <- codexdata::components(matches$deck2, starters, nicknames)
  starters <- sort(unique(c(deck_details1$starter, deck_details2$starter)))
  specs <- sort(unique(c(deck_details1$spec1, deck_details1$spec2, deck_details1$spec3,
                         deck_details2$spec1, deck_details2$spec2, deck_details2$spec3)))
  Sp <- length(specs)
  starter_specs <- outer(starters, specs, paste, sep = "/")
  spec_specs <- unlist(lapply(seq.int(Sp - 1),
                              function(n) paste0(specs[n], "/", specs[setdiff(seq.int(Sp), seq.int(n))])))
  full <- c(base,
            list(St = length(starters),
                 Sp = Sp,
                 first_starter = match(deck_details1$starter, starters),
                 first_specs1 = match(deck_details1$spec1, specs),
                 first_specs2 = match(deck_details1$spec2, specs),
                 first_specs3 = match(deck_details1$spec3, specs),
                 second_starter = match(deck_details2$starter, starters),
                 second_specs1 = match(deck_details2$spec1, specs),
                 second_specs2 = match(deck_details2$spec2, specs),
                 second_specs3 = match(deck_details2$spec3, specs),
                 starter_specs = starter_specs,
                 first_starter_specs1 = match(paste0(deck_details1$starter ,"/", deck_details1$spec1), starter_specs),
                 first_starter_specs2 = match(paste0(deck_details1$starter ,"/", deck_details1$spec2), starter_specs),
                 first_starter_specs3 = match(paste0(deck_details1$starter ,"/", deck_details1$spec3), starter_specs),
                 second_starter_specs1 = match(paste0(deck_details2$starter ,"/", deck_details2$spec1), starter_specs),
                 second_starter_specs2 = match(paste0(deck_details2$starter ,"/", deck_details2$spec2), starter_specs),
                 second_starter_specs3 = match(paste0(deck_details2$starter ,"/", deck_details2$spec3), starter_specs),
                 spec_specs = spec_specs,
                 first_spec_specs1 = match(paste(pmin(deck_details1$spec1, deck_details1$spec2),
                                                 pmax(deck_details1$spec1, deck_details1$spec2),
                                                 sep = "/"),
                                           spec_specs),
                 first_spec_specs2 = match(paste(pmin(deck_details1$spec1, deck_details1$spec3),
                                                 pmax(deck_details1$spec1, deck_details1$spec3),
                                                 sep = "/"),
                                           spec_specs),
                 first_spec_specs3 = match(paste(pmin(deck_details1$spec2, deck_details1$spec3),
                                                 pmax(deck_details1$spec2, deck_details1$spec3),
                                                 sep = "/"),
                                           spec_specs),
                 second_spec_specs1 = match(paste(pmin(deck_details2$spec1, deck_details2$spec2),
                                                  pmax(deck_details2$spec1, deck_details2$spec2),
                                                  sep = "/"),
                                            spec_specs),
                 second_spec_specs2 = match(paste(pmin(deck_details2$spec1, deck_details2$spec3),
                                                  pmax(deck_details2$spec1, deck_details2$spec3),
                                                  sep = "/"),
                                            spec_specs),
                 second_spec_specs3 = match(paste(pmin(deck_details2$spec2, deck_details2$spec3),
                                                  pmax(deck_details2$spec2, deck_details2$spec3),
                                                  sep = "/"),
                                            spec_specs),
                 StSp = length(starter_specs),
                 SpSp = length(spec_specs),
                 starters = starters,
                 specs = specs))
  if (full$StSp != full$St * full$Sp) stop("Incorrect starter_specs length")
  if (full$SpSp != choose(full$Sp, 2)) stop("Incorrect spec_specs length")
  full
}

tidy_results <- function(results, match_names, model_data) {
  tidy_results <- rstan::extract(results, permuted = TRUE)
  tst <- tidy_results[[1L]]
  tst_dim <- dim(tst)
  ord <- order(do.call(`[`,
                       c(list(tst, TRUE),
                         rep(list(1L), length(tst_dim) - 1L))))
  tidy_results <- lapply(tidy_results,
                         function(x) {
                           n_dim <- length(dim(x))
                           do.call(`[`,
                                   c(list(x, ord),
                                     rep(list(TRUE), n_dim - 1L),
                                     list(drop = FALSE)))
                         })
  tidy_results$matchup <- `colnames<-`(as.data.table(tidy_results$matchup), match_names)
  colnames(tidy_results$player) <- model_data$players
  Reduce(function(x, y) add_tidy_names(x, model_data, y[[1]], y[[2]], y[[3]]),
         list(list("player_turn"       , 2  , "players"),
              list("deck"              , 2  , "deck_names"),
              list("deck_turn"         , 2  , "deck_names"),
              list("starter"           , 2  , "starters"),
              list("starter_turn"      , 2  , "starters"),
              list("spec"              , 2  , "specs"),
              list("spec_turn"         , 2  , "specs"),
              list("starter_spec"      , 2:3, c("starters", "specs")),
              list("spec_spec"         , 2  , "spec_specs"),
              list("starter_vs_starter", 2:3, c("starters", "starters")),
              list("starter_vs_spec"   , 2:3, c("starters", "specs")),
              list("spec_vs_starter"   , 2:3, c("specs", "starters")),
              list("spec_vs_spec"      , 2:3, c("specs", "specs"))),
         init = tidy_results)
}

add_tidy_names <- function(lst, original, element_name, element_indices, original_names) {
  if (length(element_indices) != length(original_names))
    stop(paste0(element_name, ": element_indices and original_names lengths must match"))
  if (!is.element(element_name, names(lst)))
    return(lst)
  if (!identical(dim(lst[[element_name]])[element_indices],
                 unname(vapply(original_names, function(x) length(original[[x]]), integer(1)))))
    stop(paste0(element_name, ": assigned dimnames have elements with the wrong length"))
  dimnames(lst[[element_name]])[element_indices] <- lapply(original_names, function(x) original[[x]])
  lst
}
