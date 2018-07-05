context("calculateProbabilityExtremePoints")


test_that("general behavior", {
  a1 = matrix(c(-1, 1, 0, 0, -1, 1), ncol = 3L, byrow = TRUE)
  b1 = c(0, 0)
  extremes = calculateProbabilityExtremePoints(a1 = a1, b1 = b1)
  expected.extremes = matrix(c(1/3, 1/3, 1/3, 0.5, 0.5, 0, 1, 0, 0),
    ncol = 3L, byrow = TRUE)
  expect_identical(extremes, expected.extremes)
  expect_error(calculateProbabilityExtremePoints(a1 = a1),
    "both or neither")
})
