context("Nash functions")

describe("get_mean_matchup_array_from_sim()", {
  test_sim <- list(tidy_results = list(player = matrix(c(1, 0), nrow = 1L,
                                                       dimnames = list(iterations = NULL, c("player1", "player2")))),
                   vs_array = array(c(1, 0, 0, 0,
                                      0, 0, 0, 0,
                                      0, 0, 0, 0,
                                      0, 0, 0, 0),
                                    dim = c(1, 4, 4),
                                    dimnames = list(iterations = NULL,
                                                    c("Red", "Anarchy", "Blood", "Fire"),
                                                    c("Red", "Anarchy", "Blood", "Fire"))))
  it("gives the mean matchup across different iterations for all pairing in given deck set", {
    expect_identical(matrix(stats::plogis(1), dimnames = list("MonoRed", "MonoRed")),
                     get_mean_matchup_array_from_sim(test_sim,
                                                     deck_set = "custom",
                                                     deck_names = "MonoRed"))
  })
  it("removes deck duplicates if no players given", {
    expect_identical(matrix(stats::plogis(1), dimnames = list("MonoRed", "MonoRed")),
                     get_mean_matchup_array_from_sim(test_sim,
                                                     deck_set = "custom",
                                                     deck_names = c("MonoRed", "MonoRed")))
  })
  it("can add player skills", {
    expect_identical(matrix(stats::plogis(c(1, 2,
                                     0, 1)),
                            byrow = TRUE, nrow = 2L,
                            dimnames = list(c("player1", "player2"),
                                            c("player1", "player2"))),
                     get_mean_matchup_array_from_sim(test_sim,
                                                     deck_set = "custom",
                                                     deck_names = c("MonoRed", "MonoRed"),
                                                     players = c("player1", "player2")))
  })
  it("returns players in given order", {
    expect_identical(matrix(stats::plogis(c(1, 0,
                                     2, 1)),
                            byrow = TRUE, nrow = 2L,
                            dimnames = list(c("player2", "player1"),
                                            c("player2", "player1"))),
                     get_mean_matchup_array_from_sim(test_sim,
                                                     deck_set = "custom",
                                                     deck_names = c("MonoRed", "MonoRed"),
                                                     players = c("player2", "player1")))
  })
})

describe("get_nash_array_from_sim()", {
  test_sim <- list(tidy_results = list(player = matrix(c(1, 0), nrow = 1L,
                                                       dimnames = list(iterations = NULL, c("player1", "player2")))),
                   vs_array = array(c(1, 0, 0, 0,
                                      0, 0, 0, 0,
                                      0, 0, 0, 0,
                                      0, 0, 0, 0),
                                    dim = c(1, 4, 4),
                                    dimnames = list(iterations = NULL,
                                                    c("Red", "Anarchy", "Blood", "Fire"),
                                                    c("Red", "Anarchy", "Blood", "Fire"))))
  it("returns the mean Nash equilibrium over given iterations", {
    expect_equal(array(c(stats::plogis(c(1, -1, 0)),
                         rep(1, 3L)),
                       dim = c(3L, 2L, 1L),
                       dimnames = list(c("P1", "P2", "Both"), c("Win probability", "MonoRed"), NULL)),
                 get_nash_array_from_sim(test_sim,
                                         deck_set = "custom",
                                         deck_names = "MonoRed"))
  })
  it("adds player skills if given", {
    expect_equal(array(c(stats::plogis(c(1, -1, 0)),
                         1, 1, 1, 0, 0, 0),
                       dim = c(3L, 3L, 1L),
                       dimnames = list(c("P1", "P2", "Both"), c("Win probability", "player1", "player2"), NULL)),
                 get_nash_array_from_sim(test_sim,
                                         deck_set = "custom",
                                         deck_names = c("MonoRed", "MonoRed"),
                                         players = c("player1", "player2")))
    expect_equal(array(c(stats::plogis(c(1, -1, 0)),
                         0, 0, 0, 1, 1, 1),
                       dim = c(3L, 3L, 1L),
                       dimnames = list(c("P1", "P2", "Both"), c("Win probability", "player2", "player1"), NULL)),
                 get_nash_array_from_sim(test_sim,
                                         deck_set = "custom",
                                         deck_names = c("MonoRed", "MonoRed"),
                                         players = c("player2", "player1")))
  })
  it("can calculate Nash samples in parallel", {
    expect_equal(array(c(stats::plogis(c(1, -1, 0)),
                         1, 1, 1, 0, 0, 0),
                       dim = c(3L, 3L, 1L),
                       dimnames = list(c("P1", "P2", "Both"), c("Win probability", "player1", "player2"), NULL)),
                 get_nash_array_from_sim(test_sim,
                                         deck_set = "custom",
                                         deck_names = c("MonoRed", "MonoRed"),
                                         players = c("player1", "player2"),
                                         parallel = 1L))
    expect_equal(array(c(stats::plogis(c(1, -1, 0)),
                         0, 0, 0, 1, 1, 1),
                       dim = c(3L, 3L, 1L),
                       dimnames = list(c("P1", "P2", "Both"), c("Win probability", "player2", "player1"), NULL)),
                 get_nash_array_from_sim(test_sim,
                                         deck_set = "custom",
                                         deck_names = c("MonoRed", "MonoRed"),
                                         players = c("player2", "player1"),
                                         parallel = 1L))
  })
})
