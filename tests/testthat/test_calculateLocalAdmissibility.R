context("calculateLocalAdmissibility")

# setup decision problem, preference system and helpers
# dp = makeDecisionProblem(outcomes, "nature", "job")
# ps = makePreferenceSystem(dp)

dp = makeDecisionProblem(act1.dom.simple, "nature", "job")
ps = makePreferenceSystem(dp)

test_that("calculateLocalAdmissibility", {
  # check general behavior

  # the way the example act1.dom.simple was designed act1
  # dominates act2
  a1.a2 = calculateLocalAdmissibility(ps, action1 = 1L,
    action2 = 2L, p.measures.2)
  # expect that is in rel for all
  expect_true(a1.a2$min.opt >= 0)
  # expect that is in rel exists
  expect_true(a1.a2$max.opt > 0)

  a2.a1 = calculateLocalAdmissibility(ps, action1 = 2L,
    action2 = 1L, p.measures.2)
  # expect that is not in rel for all
  expect_true(a2.a1$min.opt < 0)
  # expect that is not in rel exists
  expect_true(a2.a1$max.opt <= 0)

})
# test_that("check error messages", {
#   # check error messages for infeasible probability measures
#   bad.measures = list(c(0.5, 0.5, 0.5))
#   expect_error(calculateAAdmissibility(ps, action = "1", bad.measures),
#     "length")
#   bad.measures = list(c(0.5, 0.5), c(0.8, 0.3))
#   expect_error(calculateAAdmissibility(ps, bad.measures, action = "1"),
#     "sum to 1")
#   # check sanity of action variable
#   expect_error(calculateAAdmissibility(ps, p.measures,
#     action = "bad action"), "not a valid action")
# })
