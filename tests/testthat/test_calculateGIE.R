context("calculateGIE")

# setup decision problem and preference system
dp = makeDecisionProblem(outcomes.bound, "nature", "job")
ps = makePreferenceSystem(dp)
deltas = c(0, 0.01)

test_that("check general behaviour", {
  gies.1 = sapply(deltas, function(delta) {
    calculateGIE(ps, delta, p.measures.2, action = "1")
  })
  # check bound between 0, 1
  expect_true(all(as.numeric(gies.1) <= 1 && as.numeric(gies.1) >= 0))

  # check intervals not getting wider with increasing delta
  expect_equal(gies.1[1L, ], sort(gies.1[1L, ]))
  expect_equal(gies.1[2L, ], sort(gies.1[2L, ],
    decreasing = TRUE))
})

test_that("check error messages", {
  # check if throws error when there is no worst or
  # best alternative.
  dp.unbound = makeDecisionProblem(outcomes,
    "nature", "job")
  ps.unbound = makePreferenceSystem(dp.unbound)
  expect_error(calculateGIE(ps.unbound,
    deltas[[1L]], p.measures.2, action = "1"),
    "There is either no best or worse alternative")

  # check error messages for infeasible probability measures
  expect_error(calculateGIE(ps, deltas[[1L]], bad.measures.l, action = "1"),
    "length")
  expect_error(calculateGIE(ps, deltas[[1L]], bad.measures.s, action = "1"),
    "sum to 1")
  # check sanity of action variable
  expect_error(calculateGIE(ps, deltas[[1L]], p.measures.2,
    action = "bad action"), "not a valid action")
})
