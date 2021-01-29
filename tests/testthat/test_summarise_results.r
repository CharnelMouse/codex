context("summarise results")

make_samples <- function(...) {
  as.matrix(data.frame(...))
}

describe("get_probability_positive()", {
  it("returns numeric(0) if given a NULL sample set", {
    expect_identical(get_probability_positive(NULL, character(0)), numeric(0))
  })
  it("returns numeric(0) if given a 0-length sample set", {
    expect_identical(get_probability_positive(make_samples(coef1 = numeric(0)), character(0)), numeric(0))
  })
  it("returns numeric(0) if given no coefficients to evaluate", {
    expect_identical(get_probability_positive(make_samples(coef1 = 1), NULL), numeric(0))
    expect_identical(get_probability_positive(make_samples(coef1 = 1), character(0)), numeric(0))
  })
  it("expects all given coefficients to be in the sample", {
    expect_error(get_probability_positive(make_samples(coef1 = 1), "coef2"))
    expect_error(get_probability_positive(make_samples(coef1 = 1), c("coef1", "coef2")))
  })
  it("returns as.numeric(coef > 0) if given a 1-length sample, N = 1", {
    expect_identical(get_probability_positive(make_samples(coef1 = 1), "coef1"), c(coef1 = 1))
    expect_identical(get_probability_positive(make_samples(coef1 = 0), "coef1"), c(coef1 = 0))
    expect_identical(get_probability_positive(make_samples(coef1 = -1), "coef1"), c(coef1 = 0))
  })
  it("returns as.numeric(coef > 0) if given one coefficient from a multi-coefficient sample", {
    expect_identical(get_probability_positive(make_samples(coef1 = 1, coef2 = -1), "coef1"), c(coef1 = 1))
  })
  it("returns as.numeric(coef > 0) if given multiple coefficients from a multi-coefficient sample", {
    expect_identical(get_probability_positive(make_samples(coef1 = 1, coef2 = -1), c("coef1", "coef2")),
                     c(coef1 = 1, coef2 = 0))
    expect_identical(get_probability_positive(make_samples(coef1 = 1, coef2 = -1, coef3 = 1), c("coef1", "coef3")),
                     c(coef1 = 1, coef3 = 1))
  })
  it("returns requested coefs in given order", {
    expect_identical(get_probability_positive(make_samples(coef1 = 1, coef2 = -1), c("coef2", "coef1")),
                     c(coef2 = 0, coef1 = 1))
  })
  it("returns frequency that coef > 0 if given a multi-length sample for one coefficient", {
    expect_identical(get_probability_positive(make_samples(coef1 = c(1, 1)), "coef1"), c(coef1 = 1))
    expect_identical(get_probability_positive(make_samples(coef1 = c(1, 0)), "coef1"), c(coef1 = 1/2))
    expect_identical(get_probability_positive(make_samples(coef1 = c(0, 0)), "coef1"), c(coef1 = 0))
  })
  it("returns frequency that coef > 0 for single coefficient if given a multi-length sample for multiple coefficients", {
    expect_identical(get_probability_positive(make_samples(coef1 = c(1, 1), coef2 = 1), "coef1"), c(coef1 = 1))
  })
  it("returns frequency that coef > 0 for multiple coefficients if given a multi-length sample", {
    expect_identical(get_probability_positive(make_samples(coef1 = c(1, 1), coef2 = c(1, 0)), c("coef1", "coef2")),
                     c(coef1 = 1, coef2 = 1/2))
  })
})

describe("get_n_chain_samples()", {
  it("returns 0 if given a NULL sample", {
    expect_identical(get_n_chain_samples(NULL), 0L)
  })
  it("returns 0 if given a 0-length sample", {
    expect_identical(get_n_chain_samples(make_samples(coef1 = numeric(0))), 0L)
  })
  it("returns 1 if given a 1-length sample", {
    expect_identical(get_n_chain_samples(make_samples(coef1 = 1)), 1L)
  })
  it("returns n if given an n-length sample with unique entries", {
    expect_identical(get_n_chain_samples(make_samples(coef1 = 1:10)), 10L)
    expect_identical(get_n_chain_samples(make_samples(coef1 = c(1, 1), coef2 = 1:2)), 2L)
  })
  it("doesn't count adjacently-repeated rows", {
    expect_identical(get_n_chain_samples(make_samples(coef1 = c(1, 1))), 1L)
  })
  it("does count a repeated row if there are differing entries in between", {
    expect_identical(get_n_chain_samples(make_samples(coef1 = c(1, 2, 1))), 3L)
  })
})

describe("get_chains()", {
  it("returns NULL if given a NULL samples", {
    expect_identical(get_chains(NULL), NULL)
  })
  it("returns an empty table if given an empty sample", {
    expect_identical(get_chains(make_samples(NULL)), data.table())
  })
  it("returns an empty table with column names if given a 0-length sample with columns", {
    expect_identical(get_chains(make_samples(coef1 = numeric(0))), data.table(coef1 = numeric(0)))
    expect_identical(get_chains(make_samples(coef1 = numeric(0), coef2 = numeric(0))),
                     data.table(coef1 = numeric(0), coef2 = numeric(0)))
  })
  it("puts a sample table into wide format, with iteration numbers", {
    expect_identical(get_chains(make_samples(coef1 = c(1, 2))),
                     data.table(iteration = 1:2, effect = factor("coef1"), `effect size` = as.numeric(1:2)))
    expect_equal(get_chains(make_samples(coef1 = c(1, 2), coef2 = c(4, 3))),
                 data.table(iteration = rep(1:2, times = 2),
                            effect = factor(rep(c("coef1", "coef2"), each = 2)),
                            `effect size` = c(1, 2, 4, 3)))
  })
})

describe("get_by_mean_value()", {
  empty <- data.table(effect = factor(NULL), `effect size` = numeric(0))
  it("returns NULL if given a NULL sample", {
    expect_identical(get_by_mean_value(NULL), NULL)
  })
  it("returns an empty table with effect and effect size columns if given an empty sample and no effect names", {
    expect_identical(get_by_mean_value(make_samples(NULL)), empty)
  })
  it("returns an error if given effects not contained in the sample", {
    expect_error(get_by_mean_value(make_samples(effect1 = 0), effect2))
  })
  it("returns a tall-format effects table of a given effect, with name column in factor format", {
    expect_identical(get_by_mean_value(make_samples(effect1 = 0), "effect1"),
                     data.table(effect = factor("effect1"), `effect size` = 0))
    expect_identical(get_by_mean_value(make_samples(effect1 = c(0, 1)), "effect1"),
                     data.table(effect = factor("effect1"), `effect size` = c(0, 1)))
  })
  it("returns a table of given effects, all of them by default, with name factor levels in order of mean effect size", {
    expect_identical(get_by_mean_value(make_samples(effect1 = c(0, 1), effect2 = c(2, 3))),
                     data.table(effect = factor(paste0("effect", rep(1:2, each = 2))),
                                `effect size` = c(0, 1, 2, 3)))
    expect_identical(get_by_mean_value(make_samples(effect1 = c(2, 3), effect2 = (c(0, 1)))),
                     data.table(effect = factor(paste0("effect", rep(1:2, each = 2)), levels = paste0("effect", 2:1)),
                                `effect size` = c(2, 3, 0, 1)))
    expect_identical(get_by_mean_value(make_samples(effect1 = c(0, 1), effect2 = c(2, 3), effect3 = c(3, 4)),
                                       names = paste0("effect", c(1, 3))),
                     data.table(effect = factor(paste0("effect", rep(c(1, 3), each = 2))),
                                `effect size` = c(0, 1, 3, 4)))
  })
  it("sorts by the absolute mean value if requested", {
    expect_identical(get_by_mean_value(make_samples(effect1 = 0, effect2 = -1),
                                       sort_fn = function(x) sort(abs(x))),
                     data.table(effect = factor(paste0("effect", 1:2), levels = paste0("effect", 1:2)),
                                `effect size` = c(0, -1)))
  })
})

describe("get_probability_largest", {
  it("returns an empty table if given an empty table", {
    expect_identical(get_probability_largest(data.table()),
                     data.table())
    expect_identical(get_probability_largest(data.table(effect1 = numeric(0))),
                     data.table(effect1 = numeric(0)))
  })
  it("returns a 1-vector for 1 effect if given a 1-length sample", {
    expect_identical(get_probability_largest(data.table(effect1 = 1)),
                     data.table(effect = "effect1", `effect size` = 1))
    expect_identical(get_probability_largest(data.table(effect1 = 0)),
                     data.table(effect = "effect1", `effect size` = 1))
    expect_identical(get_probability_largest(data.table(effect2 = 0)),
                     data.table(effect = "effect2", `effect size` = 1))
  })
  it("returns a 1-vector for multiple effects if given a 1-length sample", {
    expect_identical(get_probability_largest(data.table(effect1 = 1, effect2 = 0)),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(1, 0)))
    expect_identical(get_probability_largest(data.table(effect1 = -1, effect2 = 4)),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(0, 1)))
  })
  it("returns a mean 1-vector over rows of a sample", {
    expect_identical(get_probability_largest(data.table(effect1 = c(1, -1), effect2 = c(0, 4))),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(1/2, 1/2)))
    expect_identical(get_probability_largest(data.table(effect1 = c(1, 1), effect2 = c(0, 0))),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(1, 0)))
  })
  it("splits a row evenly if multiple columns are maximum value", {
    expect_identical(get_probability_largest(data.table(effect1 = 1, effect2 = 1)),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(1/2, 1/2)))
    expect_identical(get_probability_largest(data.table(effect1 = 1, effect2 = c(1, 0))),
                     data.table(effect = c("effect1", "effect2"), `effect size` = c(3/4, 1/4)))
  })
})
