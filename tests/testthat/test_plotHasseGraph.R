context("plotHasseGraph")

# FIXME: make tests with vdiffr
# setup decision problem, preference system and helpers


test_that("plotHasseGraph", {
  dp = makeDecisionProblem(outcomes, "nature", "job")
  ps = makePreferenceSystem(dp)
  gr = plotHasseGraph(ps, title = "dummy title", circular = TRUE)
  # additional params work
  # plotHasseGraph(ps, labels = letters[1:4])
})
