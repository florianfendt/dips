context("plotGIE")

test_that("plot is generated", {
  # setup decision problem and preference system
  dp.outcomes = makeDecisionProblem(outcomes.bound,
    state = "nature", action = "job")
  ps.outcomes = makePreferenceSystem(dp.outcomes)
  deltas = c(0.01, 0.015, 0.02)
  p.measures = list(c(0.5, 0.5))
  actions = c("1", "2")
  obj = plotGIE(ps.outcomes, deltas, p.measures, actions)
  # check that obj is a ggplot
  expect_true("ggplot" %in% class(obj))
})
