context("calculatePreferenceSystemConsistency")

X2 = as.data.frame(matrix(c(1, 3, 1, 2, 1, 4, 3,4), ncol = 2,
  byrow = TRUE))

X4 = as.data.frame(matrix(c(1, 2, 3, 4, 2, 1, 3, 1, 4, 2, 3, 1), ncol = 4,
  byrow = TRUE))

  X2.indiff.only = do.call("rbind", list(X2, list(X2$V2,
    X2$V1)))
  X4.indiff.only = do.call("rbind", list(X4, list(X4$V3, X4$V4, X4$V1, X4$V2)))

test_that("get I", {
  I2.strict = getI(X2)
  expect_equal(nrow(I2.strict), 0L)

  I4.strict = getI(X4)
  expect_identical(nrow(I4.strict), 0L)


  # add indifferences for 1st row
  x2.row = c(3, 1)
  x4.row = c(3, 4, 1, 2)
  X2.indiff = rbind(X2, x2.row)
  X4.indiff = rbind(X4, x4.row)

  I2.indiff = getI(X2.indiff)
  expect_equal(nrow(I2.indiff),
    nrow(X2.indiff[c(1L, nrow(X2.indiff)), ]))

  I4.indiff = getI(X4.indiff)
  expect_equal(nrow(I4.indiff),
    nrow(X4.indiff[c(1L, nrow(X4.indiff)), ]))

  # only indiff


  I2.indiff = getI(X2.indiff.only)
  expect_equal(nrow(I2.indiff), nrow(X2.indiff.only))

  I4.indiff = getI(X4.indiff.only)
  expect_equal(nrow(I4.indiff), nrow(X4.indiff.only))

})

test_that("get P", {

  P2.strict = getP(X2)
  expect_equal(nrow(P2.strict), nrow(X2))

  P4.strict = getP(X4)
  expect_identical(nrow(P4.strict), nrow(X4))

  # add indifferences for 1st row
  x2.row = c(3, 1)
  x4.row = c(3, 4, 1, 2)
  X2.indiff = rbind(X2, x2.row)
  X4.indiff = rbind(X4, x4.row)

  P2.indiff = getP(X2.indiff)
  expect_identical(P2.indiff,
    X2.indiff[-c(nrow(X2.indiff), 1L), ])

  P4.indiff = getP(X4.indiff)
  expect_equal(nrow(P4.indiff),
    nrow(X4.indiff[-c(1L, nrow(X4.indiff)), ]))

  # only indiff


  P2.indiff = getP(X2.indiff.only)
  expect_equal(nrow(P2.indiff), 0L)

  P4.indiff = getP(X4.indiff.only)
  expect_equal(nrow(P4.indiff), 0)
})

test_that("printer works", {
  dp = makeDecisionProblem(outcomes, state = "nature",
    action = "job")
  ps = makePreferenceSystem(dp)
  ps.consistency = calculatePreferenceSystemConsistency(ps)
  expect_output(print(ps.consistency),
    "consistent with granularity")
})
