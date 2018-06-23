context("customPreferenceSystem")


test_that("customPreferenceSystem", {

  # check that a user can directly setup the
  # ps himself, that will work with all
  # function that need a PreferenceSystem.

  df = data.frame(state = factor(rep(1:3, 2)),
    action = factor(rep(1:3, 2)))
  R1 = data.frame(a = c(1, 1, 1, 3, 3, 4),
    b = c(2, 5, 6, 4, 5, 6))
  R2 = data.frame(a = c(1, 1, 1, 3, 3, 4),
    b = c(2, 5, 6, 4, 5, 6),
    c = c(4, 1, 3, 1, 3, 1),
    d = c(6, 6, 5, 2, 5, 5))

  ps = list(df = df, R1 = R1, R2 = R2)
  class(ps) = "PreferenceSystem"
  ps = checkPreferenceSystem(ps)
  expect_true(class(ps) == "PreferenceSystem")

  ps.no.df = ps
  ps.no.df$df = NULL
  expect_error(checkPreferenceSystem(ps.no.df), "no data")

  ps.no.r1 = ps
  ps.no.r1$R1 = NULL
  expect_error(checkPreferenceSystem(ps.no.r1), "R1")

  ps.no.r2 = ps
  ps.no.r2$R2 = NULL
  expect_error(checkPreferenceSystem(ps.no.r2), "R2")

  ps.bad.r1 = ps
  ps.bad.r1$R1[1L, 1L] = 8L
  expect_error(checkPreferenceSystem(ps.bad.r1), "entries")

  ps.bad.r1 = ps
  ps.bad.r1$R1$c = 1:6
  expect_error(checkPreferenceSystem(ps.bad.r1), "2 columns")
})

test_that("square matrix works", {
  r1 = diag(5L)
  r2 = diag(5L^2)
  r1[1L, 2L] = 1
  r1[1L, 3L] = 1
  r2[2L, 3L] = 1
  df = data.frame(action = factor(rep(1L, times = 5L)),
    state = factor(1:5))
  ps.manual = list(df = df, R1 = r1, R2 = r2)
  class(ps.manual) = "PreferenceSystem"
  ps.manual = checkPreferenceSystem(ps.manual)
  R1.val = data.frame(c(1L, 1L), c(2L, 3L))
  colnames(R1.val) = c("1", "2")
  R2.val = data.frame("1.1" = 1L, "1.2" = 2L, "2.1" = 1L, "2.2" = 3L)
  colnames(R2.val) = c("1.1", "1.2", "2.1", "2.2")
  expect_identical(ps.manual$R1, R1.val)
  expect_identical(ps.manual$R2, R2.val)
})
