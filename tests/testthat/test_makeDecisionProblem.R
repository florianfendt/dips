context("makeDecisionProblem")


test_that("general behavior", {

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
  # check that exclude works
  dp = makeDecisionProblem(outcomes, "nature", "job", exclude = c("b1", "b2"))
  df.excluded = outcomes[, c("x", "b3", "b4", "nature", "job")]
  colnames(df.excluded) = c("x", "b3", "b4", "state", "action")
  expect_identical(dp$df, df.excluded)
  # check sanity of inputs
  no.num = outcomes[, -1L]
  expect_error(makeDecisionProblem(no.num, "nature", "job"),
    "one numeric")
  wrong.levs = cbind(outcomes, b6 = outcomes$job)
  expect_error(makeDecisionProblem(wrong.levs, "nature", "job"), "b6")
  no.state = outcomes[, -6L]
  expect_error(makeDecisionProblem(no.state, "nature", "job"), "contain")
  no.act = outcomes[, -7L]
  expect_error(makeDecisionProblem(no.act, "nature", "job"), "contain")
  # check sanity of exclude
  expect_error(makeDecisionProblem(outcomes, "nature", "job",
    exclude = "badvar"), "exclude must be present")

})

test_that("multiple numerics work", {
  outcomes$y = 1.5 * outcomes$x
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(all(dp$cardinal.vars %in% c("x", "y")))
})
