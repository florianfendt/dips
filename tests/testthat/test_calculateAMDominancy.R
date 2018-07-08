context("calculateAMDominancy")

# setup decision problem, preference system and helpers
dp = makeDecisionProblem(act1.dom, "nature", "job")
ps = makePreferenceSystem(dp)

test_that("calculateAMDominancy", {
  # check general behavior
  am.dom = calculateAMDominancy(ps, 1L, p.measures.2)
  # the way the example act1.dom was designed act 1
  # dominates the other two acts
  expect_true(all(am.dom >= 0))
  # Other two acts should not be A-Admissible
  am.dom = calculateAMDominancy(ps, 2L, p.measures.2)
  expect_true(!all(am.dom >= 0))
  am.dom = calculateAMDominancy(ps, 3L, p.measures.2)
  expect_true(!all(am.dom >= 0))
})
