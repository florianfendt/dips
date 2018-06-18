context("calculateAAdmissibility")

# setup decision problem, preference system and helpers
dp = makeDecisionProblem(act1.dom, "nature", "job")
ps = makePreferenceSystem(dp)

test_that("calculateAAdmissibility", {
  # check general behavior
  a.admiss = calculateAAdmissibility(ps, 1L, p.measures.2)
  # the way the example act1.dom was designed act 1
  # dominates the other two acts
  expect_true(a.admiss > 0)
  # Other two acts should not be A-Admissible
  a.admiss = calculateAAdmissibility(ps, 2L, p.measures.2)
  expect_true(a.admiss <= 0)
  a.admiss = calculateAAdmissibility(ps, 3L, p.measures.2)
  expect_true(a.admiss <= 0)
})

test_that("check error messages", {
  # check error messages for infeasible probability measures
  bad.measures = list(c(0.5, 0.5, 0.5))
  expect_error(calculateAAdmissibility(ps, action = "1", bad.measures),
    "length")
  bad.measures = list(c(0.5, 0.5), c(0.8, 0.3))
  expect_error(calculateAAdmissibility(ps, bad.measures, action = "1"),
    "sum to 1")
  # check sanity of action variable
  expect_error(calculateAAdmissibility(ps, p.measures,
    action = "bad action"), "not a valid action")
})
