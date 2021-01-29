context("data/results preparation")

test_that("add_tidy_names adds names", {
  lst <- list(simple = array(1:10))
  orig <- list(simple = letters[1:10])
  expect_identical(add_tidy_names(lst, orig, "simple", 1L, "simple"),
                   list(simple = array(1:10, dimnames = list(letters[1:10]))))
})
