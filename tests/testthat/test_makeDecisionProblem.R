context("makeDecisionProblem")


test_that("general behavior", {

  # setup decision problem
  dp = makeDecisionProblem(outcomes, "nature", "job")
  #check print method
  expect_output(print(dp), "Decision Problem")
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

test_that("numerics work for different numbers of variables", {
  # Check it works for 2 numerics
  outcomes$y = 1.5 * outcomes$x
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(all(dp$cardinal.vars %in% c("x", "y")))

  # Check it works for 0 numerics
  outcomes$x = NULL
  outcomes$y = NULL
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(is.null(dp$cardinal.vars))
  expect_true(is.null(dp$cardinal.information))
})

test_that("ordinals work for different numbers of variables", {
  # Check it works for  multiple ordinals
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(all(dp$ordinal.vars %in% c("b1", "b2", "b3", "b4")))

  # Check it works for  1 ordinal
  outcomes$b1 = NULL
  outcomes$b2 = NULL
  outcomes$b3 = NULL
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(all(dp$ordinal.vars == "b4"))
  expect_true(length(dp$ordinal.information[[1L]]) == 1L)

  # Check it works for 0 ordinals
  outcomes$b4 = NULL
  dp = makeDecisionProblem(outcomes, "nature", "job")
  expect_true(is.null(dp$ordinal.vars))
  expect_true(is.null(dp$ordinal.information))
})
