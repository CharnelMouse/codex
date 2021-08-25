theme_codex <- ggplot2::theme_dark() +
  ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, colour = "grey13"),
        plot.background = ggplot2::element_rect(fill = "grey13", colour = "grey13"),
        axis.ticks = ggplot2::element_line(colour = "white"),
        strip.background = ggplot2::element_rect(fill = "grey13", colour = NA),
        legend.background = ggplot2::element_rect(fill = "grey13", colour = NA),
        text = ggplot2::element_text(colour = "white"),
        axis.text = ggplot2::element_text(colour = "white"),
  )
ggplot2::theme_set(theme_codex)

get_probability_positive <- function(samples, coefs) {
  if (length(samples) == 0 | length(coefs) == 0)
    return(numeric(0))
  colMeans(samples[, coefs, drop = FALSE] > 0)
}

get_n_chain_samples <- function(samples) {
  if (is.null(samples))
    return(0L)
  (nrow(samples) > 0) + sum(apply(samples[-1, , drop = FALSE] - samples[-nrow(samples), , drop = TRUE],
                                  1,
                                  function(x) any(x != 0)))
}

plot_marginal <- function(samples) {
  tidyr::gather(as.data.frame(samples), "effect", "effect size", dplyr::everything(), factor_key = TRUE) %>%
    ggplot2::ggplot(ggplot2::aes(.data$`effect size`)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ effect) +
    ggplot2::xlab("effect size")
}

plot_chain <- function(samples) {
  get_chains(samples) %>%
    ggplot2::ggplot(ggplot2::aes(.data$iteration, .data$`effect size`)) +
    ggplot2::geom_line() +
    ggplot2::xlab("iteration") +
    ggplot2::ylab("effect size") +
    ggplot2::facet_wrap(~ .data$effect)
}

get_chains <- function(samples) {
  if (is.null(samples))
    return(NULL)
  if (nrow(samples) == 0L) {
    if (ncol(samples) == 0L)
      data.table()
    else
      as.data.table(samples)[, lapply(.SD, as.numeric)]
  }else
    melt(cbind(iteration = seq.int(nrow(samples)), as.data.table(samples)),
         id.vars = "iteration", variable.name = "effect", value.name = "effect size", variable.factor = TRUE)
}

#' Get parameters by mean sampled value
#'
#' @param samples a tibble, with a column of sampled values for each model parameter
#' @param names a character vector, giving names of parameters to sort, defaults to all parameters
#' @param sort_fn a function used to sort the mean parameter values, defaults to sort()
#'
#' @return a character vector, giving parameters sorted in order of mean sampled value.
#' @export
get_by_mean_value <- function(samples, names = colnames(samples), sort_fn = sort) {
  if (is.null(samples))
    return(NULL)
  if (length(names) == 0)
    return(data.table(effect = factor(NULL), `effect size` = numeric(0)))
  filtered <- samples[, names, drop = FALSE]
  df <- melt(as.data.table(filtered), measure.vars = names,
             variable.name = "effect", value.name = "effect size", variable.factor = TRUE)
  levels <- names(sort_fn(colMeans(filtered)))
  df$effect <- factor(df$effect, levels)
  df
}

#' Make horizontal violin plot with given titles
#'
#' @param effects a tibble, containing the columns to be plotted
#' @param title plot title
#' @param subtitle optional plot subtitle
#' @param key_name an unquoted expression, naming the grouping column
#' @param value_name an unquoted expression, naming the values column
#'
#' @return a ggplot object.
#' @export
plot_flipped_effects_violin <- function(effects, title, subtitle = NULL, key_name = effect, value_name = `effect size`) {
  ggplot2::ggplot(effects, ggplot2::aes(!!rlang::enquo(key_name), !!rlang::enquo(value_name))) +
    ggplot2::geom_violin() +
    ggplot2::coord_flip() +
    ggplot2::ggtitle(title, subtitle)
}

#' Plot player skill distributions, sorted by mean skill
#'
#' @param skills A data frame, containing player skill samples in tall format.
#' @param turn A character, describing the type of player skill.
#' @param subtitle A character, giving the plot subtitle. Set to `waiver()` to
#'   have no subtitle
#'
#' @return A ggplot.
#' @export
plot_player_skill <- function(skills, turn = c("overall", "first", "second"), subtitle = "Mean-effects models") {
  turn <- match.arg(turn)
  plt <- ggplot2::ggplot(skills, ggplot2::aes(.data$player, .data$`player skill`, fill = .data$mean)) +
    ggplot2::geom_violin(position = "identity", colour = NA) +
    ggplot2::coord_flip() +
    ggplot2::xlab("player") +
    ggplot2::ylab("skill") +
    ggplot2::scale_fill_viridis_c(guide = "none", option = "B") +
    ggplot2::theme(legend.position = "bottom")
  base_title <- "Player skill"
  add_title <- switch(turn,
                      overall = "",
                      first = " as first player",
                      second = " as second player")
  plt + ggplot2::ggtitle(paste0(base_title, add_title), subtitle)
}

#' Make horizontal boxplot with given titles
#'
#' @inheritParams plot_flipped_effects_violin
#'
#' @return a ggplot object.
#' @export
plot_flipped_effects_box <- function(effects, title, subtitle = NULL, key_name = effect, value_name = `effect size`) {
  ggplot2::ggplot(effects, ggplot2::aes(!!rlang::enquo(key_name), !!rlang::enquo(value_name))) +
    ggplot2::geom_boxplot() +
    ggplot2::coord_flip() +
    ggplot2::ggtitle(title, subtitle)
}

#' Get probability of parameters being largest
#'
#' @param samples a tibble, containing a column for each parameter
#'
#' @return a data.table, with an \code{effect} grouping column and an
#'   \code{`effect size`} probability column
#' @export
get_probability_largest <- function(samples) {
  if (nrow(samples) == 0)
    return(samples)
  maximums <- apply(samples, 1, max)
  is_max <- ifelse(samples == maximums, 1L, 0L)
  probs <- colMeans(is_max/rowSums(is_max))
  data.table(effect = names(probs), `effect size` = probs)
}

#' Plot probabilities of parameters being largest, sorted by probability
#'
#' @param probabilities a tibble, containing an \code{effect} grouping column
#'   and an \code{`effect size`} probability column
#'
#' @return a ggplot object
#' @export
plot_probability_largest <- function(probabilities) {
  probabilities %>%
    dplyr::mutate(effect = factor(.data$effect, levels = .data$effect[order(.data$`effect size`)])) %>%
    ggplot2::ggplot(ggplot2::aes(.data$effect, .data$`effect size`)) +
    ggplot2::geom_point() +
    ggplot2::coord_flip()
}

#' Plot rank distribution for parameters
#'
#' @param samples a tibble, containing a column of sample values for each parameter
#' @param group_name a string, giving the name of the ranked group for use in the plot title
#' @param model_name a string, giving the name/description of the model used, to use as the plot subtitle
#'
#' @return a ggplot object
#' @export
plot_rank <- function(samples, group_name, model_name = ggplot2::waiver()) {
  ranks <- dplyr::mutate(tibble::as_tibble(samples), sample = seq.int(nrow(samples))) %>%
    tidyr::gather("key", "value", -.data$sample) %>%
    dplyr::group_by(.data$sample) %>%
    dplyr::mutate(rank = rank(-.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$key, .data$rank) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(freq = .data$count/sum(.data$count)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(.data$key, .data$rank, fill = list(freq = 0))
  best <- dplyr::group_by(ranks, .data$key) %>%
    dplyr::summarise(rank = stats::weighted.mean(.data$rank, .data$freq)) %>%
    dplyr::arrange(.data$rank) %>%
    dplyr::pull(.data$key)

  ggplot2::ggplot(dplyr::mutate(ranks, key = factor(.data$key, levels = best)), ggplot2::aes(.data$rank, .data$freq)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(. ~ .data$key) +
    ggplot2::xlab("rank") +
    ggplot2::ylab("frequency") +
    ggplot2::ggtitle(paste("Rank of", group_name), model_name)
}

#' Extract rank distribution for player skills
#'
#' @param x A data frame, giving sampled player skills in wide format.
#'
#' @return A tibble, giving sampled player skill ranks, with levels in order of average rank.
#' @export
extract_skill_ranks <- function(x) {
  x %>%
    dplyr::mutate(player = factor(.data$player, rev(levels(.data$player)))) %>%
    dplyr::group_by(.data$sample, .data$model) %>%
    dplyr::mutate(rank = rank(-.data$`player skill`)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$player, .data$model, .data$rank) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(freq = .data$count/sum(.data$count)) %>%
    dplyr::ungroup()
}
