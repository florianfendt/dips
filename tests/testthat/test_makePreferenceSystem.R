context("makePreferenceSystem")


test_that("makePreferenceSystem", {

  # setup decision problem and preference system
  dp = makeDecisionProblem(outcomes, "nature", "job")
  ps = makePreferenceSystem(dp)

  # we need to order here for comparison later
  R1 = ps$R1[order(ps$R1$row), ]
  rownames(R1) = seq_len(nrow(R1))
  R2 = ps$R2[order(ps$R2$row, ps$R2$row.1), ]
  rownames(R2) = seq_len(nrow(R2))

  # initialize true R1, R2 (calculated by hand)
  R1.val = data.frame(row = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 4L, 5L),
    col = c(2L, 3L, 4L, 5L, 6L, 4L, 5L, 5L, 4L))
  R1.val = R1.val[order(R1.val$row), ]

  R2.val = data.frame(row = c(rep(1L, 16L), rep(2L, 6L), 4L, 5L),
    col = c(rep(2L, 4L), rep(4L, 6L), rep(5L, 6L), rep(4L, 3L),
      rep(5L, 4L), 4L),
    row.1 = c(2L, 2L, 4L, 5L, 1L, 1L, 2L, 2L, 4L, 5L, 1L, 1L, 2L, 2L, 4L, 5L,
      2L, 4L, 5L, 2L, 4L, 5L, 5L, 4L),
    col.1 = c(4L, 5L, 5L, 4L, 2L, 5L, 4L, 5L, 5L, 4L, 2L, 4L, 4L, 5L, 5L, 4L,
      5L, 5L, 4L, 4L, 5L, 4L, 4L, 5L))
  R2.val = R2.val[order(R2.val$row, R2.val$row.1), ]
  rownames(R2.val) = seq_len(nrow(R2.val))

  # run the test
  expect_equal(R1, R1.val)
  expect_equal(R2, R2.val)
})
