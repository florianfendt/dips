context("drawHasseGraph")

# setup decision problem, preference system and helpers
dp = makeDecisionProblem(act1.dom.simple, "nature", "job")
ps = makePreferenceSystem(dp)

test_that("drawHAsseGraph", {
  drawHasseGraph(ps)
  # additional params work
  drawHasseGraph(ps, labels = letters[1:4])
})
