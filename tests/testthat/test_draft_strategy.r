context("draft strategy")

describe("draft_move()", {
  sample_component_names <- c(
    "Green", "Red",
    "Anarchy", "Balance", "Blood", "Feral", "Fire", "Growth"
  )
  vs_array <- array(
    0,
    dim = c(8, 8),
    dimnames = list(sample_component_names, sample_component_names)
  )
  extended_vs_array <- array(
    0,
    dim = c(9, 9),
    dimnames = list(
      c(sample_component_names, "Law"),
      c(sample_component_names, "Law")
    )
  )
  extended_vs_array["Law", "Green"] <- -1/2
  it("expects no components to be picked more than once across given moves", {
    expect_error(
      draft_move(
        vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = c("Feral", "Growth"),
        second_P1_move = "Feral"
      ),
      "^starters/specs cannot be picked more than once$"
    )
  })
  it("expects NULL inputs to all be at end to respect draft order", {
    expect_error(
      draft_move(
        vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = NULL,
        second_P1_move = "Fire"
      ),
      "^must preserve draft order: no non-NULL inputs after NULL$"
    )
  })
  it("returns simple matchup table if given full picks", {
    expect_identical(
      draft_move(
        vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = c("Feral", "Growth"),
        second_P1_move = "Fire"
      ),
      data.table::data.table(
        P1 = "MonoRed",
        P2 = "MonoGreen",
        `P1 win probability` = 1/2
      )
    )
  })
  it("gives matchup for all P1 2nd picks if missing, sorted by P1 win prob", {
    expect_identical(
      draft_move(
        vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = c("Feral", "Growth"),
        second_P1_move = NULL
      ),
      data.table::data.table(
        second_P1_move = "Fire",
        P1 = "MonoRed",
        P2 = "MonoGreen",
        `P1 win probability` = 1/2
      )
    )
    expect_identical(
      draft_move(
        extended_vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = c("Feral", "Growth"),
        second_P1_move = NULL
      ),
      data.table::data.table(
        second_P1_move = c("Fire", "Law"),
        P1 = c("MonoRed", "[Anarchy/Blood]/Law"),
        P2 = "MonoGreen",
        `P1 win probability` = c(1/2, plogis(-1/2))
      )
    )
  })
  it("gives P1-best matchup for each P2 2nd pick, if first missing set, sorted for P2", {
    expect_identical(
      draft_move(
        extended_vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = c("Red", "Anarchy", "Blood"),
        second_P2_move = NULL,
        second_P1_move = NULL
      ),
      data.table::data.table(
        second_P2_move = c(
          "Feral/Fire",
          "Feral/Growth",
          "Feral/Law", "Feral/Law",
          "Fire/Growth",
          "Fire/Law", "Fire/Law",
          "Growth/Law", "Growth/Law"
        ),
        second_P1_move = c(
          "Growth",
          "Fire",
          "Fire", "Growth",
          "Feral",
          "Feral", "Growth",
          "Feral", "Fire"
        ),
        P1 = c(
          "[Anarchy/Blood]/Growth",
          "MonoRed",
          "MonoRed", "[Anarchy/Blood]/Growth",
          "[Anarchy/Blood]/Feral",
          "[Anarchy/Blood]/Feral", "[Anarchy/Blood]/Growth",
          "[Anarchy/Blood]/Feral", "MonoRed"
        ),
        P2 = c(
          "[Balance/Feral]/Fire",
          "MonoGreen",
          "[Balance/Feral]/Law", "[Balance/Feral]/Law",
          "[Balance/Growth]/Fire",
          "[Balance]/Fire/Law", "[Balance]/Fire/Law",
          "[Balance/Growth]/Law", "[Balance/Growth]/Law"
        ),
        `P1 win probability` = plogis(
          c(
            0,
            0,
            0, 0,
            0,
            0, 0,
            0, 0
          )
        )
      )
    )
  })
  it("gives P2-best matchup for each 1st P1 pick, if first missing set, sorted for P1", {
    expect_identical(
      draft_move(
        extended_vs_array,
        first_P2_move = c("Green", "Balance"),
        first_P1_move = NULL,
        second_P2_move = NULL,
        second_P1_move = NULL
      ),
      data.table(
        first_P1_move = c(
          "Red/Anarchy/Blood",
          "Red/Anarchy/Feral",
          "Red/Anarchy/Fire",
          "Red/Anarchy/Growth",
          "Red/Anarchy/Law",
          "Red/Blood/Feral",
          "Red/Blood/Fire",
          "Red/Blood/Growth",
          "Red/Blood/Law",
          "Red/Feral/Fire",
          "Red/Feral/Growth",
          "Red/Feral/Law",
          "Red/Fire/Growth",
          "Red/Fire/Law",
          "Red/Growth/Law"
        ),
        first_P1_sets = list(
          c("Red", "Anarchy", "Blood"),
          c("Red", "Anarchy", "Feral"),
          c("Red", "Anarchy", "Fire"),
          c("Red", "Anarchy", "Growth"),
          c("Red", "Anarchy", "Law"),
          c("Red", "Blood", "Feral"),
          c("Red", "Blood", "Fire"),
          c("Red", "Blood", "Growth"),
          c("Red", "Blood", "Law"),
          c("Red", "Feral", "Fire"),
          c("Red", "Feral", "Growth"),
          c("Red", "Feral", "Law"),
          c("Red", "Fire", "Growth"),
          c("Red", "Fire", "Law"),
          c("Red", "Growth", "Law")
        )
      )[,
        draft_move(
          extended_vs_array,
          c("Green", "Balance"),
          first_P1_move = first_P1_sets[[1]],
          second_P2_move = NULL,
          second_P1_move = NULL
        ),
        by = "first_P1_move"
        ][
          order(
            -`P1 win probability`,
            first_P1_move,
            second_P2_move,
            second_P1_move
          )
          ]
    )
  })
  it("gives P1-best matchup for each 1st P2 pick, if no sets given, sorted for P2", {
    full_extended <- draft_move(
      extended_vs_array,
      first_P2_move = NULL,
      first_P1_move = NULL,
      second_P2_move = NULL,
      second_P1_move = NULL
    )
    # Should never see Law vs. Green
    expect_false(
      full_extended[
        ,
        any(
          grepl("Law", P1) &
            grepl("^\\[(Balance|Feral|Growth)|Green", P2)
        )
        ]
    )
    expect_identical(
      unique(full_extended$`P1 win probability`),
      0.5
    )
    expect_false(is.unsorted(full_extended$first_P2_move))
    expect_true(
      all(table(full_extended$P1) == 36)
    )
    # All appearing deck pairs appear for all 3^2 pick orders
    expect_true(
      all(
        table(paste(full_extended$P1, full_extended$P2) == 9)
      )
    )
    P2_counts <- table(full_extended$P2)
    # If P1 choices include sub-optimal Law vs. Green, then only copies of
    # single P1 deck
    expect_true(
      all(
        P2_counts[
          grepl("(^\\[(Balance|Feral|Growth))|Green", names(P2_counts)) &
            !grepl("Law", names(P2_counts))
          ] == 9
      )
    )
    # Otherwise, P1 has four specs to choose from, so four decks
    expect_true(
      all(
        P2_counts[
          !grepl("(^\\[(Balance|Feral|Growth))|Green", names(P2_counts)) |
            grepl("Law", names(P2_counts))
          ] == 4*9
      )
    )
  })
})
