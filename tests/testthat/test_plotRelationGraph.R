context("plotRelationGraph")

# FIXME: make tests with vdiffr
# setup decision problem, preference system and helpers


test_that("plotRelationGraph", {
  dp = makeDecisionProblem(outcomes, "nature", "job")
  ps = makePreferenceSystem(dp)
  gr = plotRelationGraph(ps, title = "dummy title")
})
