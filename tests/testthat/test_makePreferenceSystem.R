context("makePreferenceSystem")


test_that("makePreferenceSystem", {

  # setup decision problem and preference system
  dp = makeDecisionProblem(outcomes, "nature", "job")
  ps = makePreferenceSystem(dp)

  #check print method
  expect_output(print(ps), "Preference System")

  # we need to order here for comparison later
  R1 = ps$R1[order(ps$R1[, 1L]), ]
  rownames(R1) = seq_len(nrow(R1))
  R2 = ps$R2[order(ps$R2[, 1L], ps$R2[, 3L]), ]
  rownames(R2) = seq_len(nrow(R2))

  # initialize true R1, R2 (calculated by hand)
  R1.val = data.frame("1" = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 4L, 5L),
    "2" = c(2L, 3L, 4L, 5L, 6L, 4L, 5L, 5L, 4L))
  R1.val = R1.val[order(R1.val[, 1L]), ]
  names(R1.val) = 1:2

  R2.val = data.frame("1" = c(rep(1L, 16L), rep(2L, 6L), 4L, 5L),
    "2" = c(rep(2L, 4L), rep(4L, 6L), rep(5L, 6L), rep(4L, 3L),
      rep(5L, 4L), 4L),
    "3" = c(2L, 2L, 4L, 5L, 1L, 1L, 2L, 2L, 4L, 5L, 1L, 1L, 2L, 2L, 4L, 5L,
      2L, 4L, 5L, 2L, 4L, 5L, 5L, 4L),
    "4" = c(4L, 5L, 5L, 4L, 2L, 5L, 4L, 5L, 5L, 4L, 2L, 4L, 4L, 5L, 5L, 4L,
      5L, 5L, 4L, 4L, 5L, 4L, 4L, 5L))
  R2.val = R2.val[order(R2.val[, 1L], R2.val[, 3L]), ]
  rownames(R2.val) = seq_len(nrow(R2.val))
  names(R2.val) = c("1.1", "1.2", "2.1", "2.2")

  # run the test
  expect_equal(R1, R1.val)
  expect_equal(R2, R2.val)

  # check error for wrong param type
  expect_error(makePreferenceSystem(outcomes),
    "Wrong parameter")
})

test_that("numerics work for different numbers of variables", {
  # multiple numerics work
  outcomes2 = outcomes
  outcomes2$y = 1.5 * outcomes2$x
  dp2 = makeDecisionProblem(outcomes2, "nature", "job")
  ps2 = makePreferenceSystem(dp2)
  dp = makeDecisionProblem(outcomes, "nature", "job")
  ps = makePreferenceSystem(dp)
  # should be identical since y is only monotone trafo of x
  expect_identical(ps2$R1, ps$R1)
  expect_identical(ps2$R2, ps$R2)

  # no numerics work
  outcomes.no = outcomes
  outcomes.no$x = NULL
  dp3 = makeDecisionProblem(outcomes.no, "nature", "job")
  ps3 = makePreferenceSystem(dp3)
  # act 1 dominates all so should be 5 times in 1st column of R1
  expect_true(nrow(ps3$R1[ps3$R1[, 1L] == 1L, ]) == 5L)

})

test_that("ordinals work for different numbers of variables", {

  # Check it works for 1 ordinal
  outcomes.ord = outcomes
  outcomes.ord$b1 = NULL
  outcomes.ord$b2 = NULL
  outcomes.ord$b3 = NULL
  dp.ord = makeDecisionProblem(outcomes.ord, "nature", "job")
  ps.ord = makePreferenceSystem(dp.ord)
  # act 1 dominates all so should be 5 times in R1
  expect_true(nrow(ps.ord$R1[ps.ord$R1[, 1L] == 1L, ]) == 5L)

  # Check it works for 0 ordinals
  outcomes.ord$b4 = NULL
  dp.ord = makeDecisionProblem(outcomes.ord, "nature", "job")
  ps.ord = makePreferenceSystem(dp.ord)
  # act 1 dominates all so should be 5 times in R1
  expect_true(nrow(ps.ord$R1[ps.ord$R1[, 1L] == 1L, ]) == 5L)

})
