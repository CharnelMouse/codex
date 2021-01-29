#' Suggest picks for the Codex forum draft format
#'
#' Given opposed component pair effects, determine the strength of possible sets
#' of picks for the next step in a forum draft.
#'
#' The forum draft works as follows: Player two begins by choosing a starter and
#' a spec. Player one then chooses a starter and two specs. Player two chooses
#' two specs, then player one chooses their final spec.
#'
#' Unlike other formats, players are not allowed to both choose the same starter
#' or spec, so a player's picks block their opponent's options. To prevent
#' someone being completely locked out of valid choices, decks are not required
#' to have any specs associated with the chosen starter.
#'
#' A \code{NULL} value for a pick step indicates that the step has not already
#' occurred. The function decides on values for the missing steps.
#'
#' @param vs_array An 2-D array of opposed component pair effects, for a single
#'   sample.
#' @param first_P2_move Either NULL, or a length-2 character vector, giving a
#'   starter and a spec.
#' @param first_P1_move Either NULL, or a length-3 character vector, giving a
#'   starter and two specs.
#' @param second_P2_move Either NULL, or a length-2 character vector, giving two
#'   specs.
#' @param second_P1_move Either NULL, or a length-1 character vector, giving a
#'   spec.
#' @param starters A data.table, containing \code{spec} and \code{starter}
#'   columns. Defaults to the official starters and specs in
#'   \code{codexdata::starters}.
#' @param nicknames A data.table containing \code{nickname} and \code{name}
#'   columns. Defaults to the nicknames table in \code{codexdata}.
#'
#' @return A data.table, with at least one row for each possible next set of
#'   picks. Each row represents a possible ensuing series of picks to the end of
#'   selection, such that the ensuing picks are optimal for their respective
#'   player, in the minimax sense for win probability, assuming players are of
#'   equal skill. For example, if the next set of picks is the first set for
#'   player 2, then, for each possible such pick, there is a row for each
#'   following set of second picks, such that player two's second pick minimises
#'   the P1 win probability, bearing in mind possible ensuring second picks for
#'   player one, and player one's second pick maximises it. there is a row for
#'   each proceeding second pick for player 1 that maximises the win probability
#'   for player 1. This is done for each step in turn, so all picks after the
#'   first are optimal for their respective player. The table has a column for
#'   each remaining pick step, following by \code{P1} and \code{P2} columns for
#'   the resulting decks, and a \code{`P1 win probability`} column. The table is
#'   sorted in descending P1 win probability, then alphabetically by picks,
#'   starting with the first remaining step.
#' @export
draft_move <- function(
  vs_array,
  first_P2_move,
  first_P1_move,
  second_P2_move,
  second_P1_move,
  starters = codexdata::starters[base == "yes"],
  nicknames = codexdata::nicknames
) {
  if (
    anyDuplicated(
      c(
        first_P2_move,
        first_P1_move,
        second_P2_move,
        second_P1_move
      )
    )
  ) {
    stop("starters/specs cannot be picked more than once")
  }
  nulls <- vapply(
    list(
      first_P2_move,
      first_P1_move,
      second_P2_move,
      second_P1_move
    ),
    is.null,
    logical(1)
  )
  if (any(nulls[-1] < nulls[-4])) {
    stop("must preserve draft order: no non-NULL inputs after NULL")
  }
  first_null <- match(TRUE, nulls)
  result <- switch(
    as.character(first_null),
    `1` = {
      available_starters <- setdiff(
        rownames(vs_array),
        starters$spec
      )
      available_specs <- setdiff(
        rownames(vs_array),
        starters$starter
      )
      available_grid <- as.data.table(
        expand.grid(
          available_starters,
          available_specs,
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
      )
      available_picks <- available_grid[, paste(Var1, Var2, sep = "/")]
      available_sets <- lapply(
        seq.int(nrow(available_grid)),
        function(n) {
          unname(unlist(available_grid[n, , drop = TRUE]))
        }
      )
      data.table(
        first_P2_move = available_picks,
        first_P2_set = available_sets
      )[,
        draft_move(
          vs_array,
          first_P2_move = first_P2_set[[1]],
          first_P1_move = NULL,
          second_P2_move = NULL,
          second_P1_move = NULL
        )[
          `P1 win probability` == max(`P1 win probability`)
          ],
        by = "first_P2_move"
        ]
    },
    `2` = {
      available_starters <- setdiff(
        rownames(vs_array),
        c(first_P2_move,
          starters$spec)
      )
      available_specs <- setdiff(
        rownames(vs_array),
        c(first_P2_move,
          starters$starter)
      )
      available_grid <- as.data.table(
        expand.grid(
          available_starters,
          utils::combn(
            available_specs,
            2,
            paste,
            collapse = "/"
          ),
          KEEP.OUT.ATTRS = FALSE,
          stringsAsFactors = FALSE
        )
      )[,
        c("Var2", "Var3") := tstrsplit(Var2, "/")][]
      available_picks <- available_grid[, paste(Var1, Var2, Var3, sep = "/")]
      available_sets <- lapply(
        seq.int(nrow(available_grid)),
        function(n) {
          unname(unlist(available_grid[n, , drop = TRUE]))
        }
      )
      data.table(
        first_P1_move = available_picks,
        first_P1_set = available_sets
      )[,
        draft_move(
          vs_array,
          first_P2_move,
          first_P1_move = first_P1_set[[1]],
          second_P2_move = NULL,
          second_P1_move = NULL
        )[
          `P1 win probability` == min(`P1 win probability`)
        ],
        by = "first_P1_move"
        ]
    },
    `3` = {
      available <- setdiff(
        rownames(vs_array),
        c(first_P2_move,
          first_P1_move,
          starters$starter)
      )
      available_sets <- utils::combn(available, 2, simplify = FALSE)
      available_picks <- vapply(
        available_sets,
        paste,
        character(1),
        collapse = "/"
      )
      data.table(
        second_P2_move = available_picks,
        second_P2_set = available_sets
      )[,
        draft_move(
          vs_array,
          first_P2_move,
          first_P1_move,
          second_P2_move = second_P2_set[[1]],
          second_P1_move = NULL
        )[
          `P1 win probability` == max(`P1 win probability`)
          ],
        by = "second_P2_move"
        ]
    },
    `4` = {
      second_deck <- codexdata::standardise_deck_name(
        paste(
          c(second_P2_move, first_P2_move[2:1]),
          collapse = "/"
        ),
        starters = starters,
        nicknames = nicknames
      )
      available <- setdiff(
        rownames(vs_array),
        c(
          first_P2_move,
          first_P1_move,
          second_P2_move,
          starters$starter
        )
      )
      data.table(
        second_P1_move = available
      )[,
        .(second_P1_move,
          P1 = vapply(
            second_P1_move,
            function(move) {
              codexdata::standardise_deck_name(
                paste(
                  c(move, first_P1_move[3:1]),
                  collapse = "/"
                ),
                starters = starters,
                nicknames = nicknames
              )
            },
            character(1)
          ),
          P2 = second_deck,
          `P1 win probability` = vapply(
            second_P1_move,
            function(move) {
              stats::plogis(
                sum(
                  vs_array[
                    c(first_P1_move, move),
                    c(first_P2_move, second_P2_move)
                    ]
                )
              )
            },
            numeric(1)
          ))
        ]
    },
    `NA` = {
      second_deck <- codexdata::standardise_deck_name(
        paste(
          c(second_P2_move, first_P2_move[2:1]),
          collapse = "/"
        ),
        starters = starters,
        nicknames = nicknames
      )
      first_deck <- codexdata::standardise_deck_name(
        paste(
          c(second_P1_move, first_P1_move[3:1]),
          collapse = "/"
        ),
        starters = starters,
        nicknames = nicknames
      )
      data.table(
        P1 = first_deck,
        P2 = second_deck,
        `P1 win probability` = stats::plogis(
          sum(
            vs_array[
              c(first_P1_move, second_P1_move),
              c(first_P2_move, second_P2_move)
              ]
          )
        )
      )
    }
  )
  switch(
    as.character(first_null),
    `1` = result[
      order(
        `P1 win probability`,
        first_P2_move,
        first_P1_move,
        second_P2_move,
        second_P1_move
      )
    ],
    `2` = result[
      order(
        -`P1 win probability`,
        first_P1_move,
        second_P2_move,
        second_P1_move
      )
      ],
    `3` = result[order(`P1 win probability`, second_P2_move, second_P1_move)],
    `4` = result[order(-`P1 win probability`, second_P1_move,)],
    `NA` = result
  )
}
