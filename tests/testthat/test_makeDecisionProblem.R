context("makeDecisionProblem")


test_that("makeDecisionProblem", {

  # setup decision problem
  dp = makeDecisionProblem(outcomes, "nature", "job")
  # check sanity of return objects
  expect_true(class(dp) == "DecisionProblem")
  df = dp$df
  expect_true(all(c("state", "action") %in% colnames(df)))
  expect_true(is.factor(df$state))
  expect_true(is.factor(df$action))
  expect_true(class(dp$ordinal.information) == "list")
  expect_true(class(dp$cardinal.information) == "list")
  # check sanity of inputs
  no.num = df[, -1L]
  expect_error(makeDecisionProblem(no.num, "nature", "job"),
    "one numeric")
  wrong.levs = cbind(outcomes, b6 = outcomes$job)
  expect_error(makeDecisionProblem(wrong.levs, "nature", "job"), "one numeric")
  no.state = outcomes[, -6L]
  expect_error(makeDecisionProblem(no.state, "nature", "job"), "contain")
  no.act = outcomes[, -7L]
  expect_error(makeDecisionProblem(no.state, "nature", "job"), "contain")
})
