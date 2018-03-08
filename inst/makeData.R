dn = "../data"
stopifnot(isDirectory(dn))

outcomes = data.frame(x = c(5000, 2700, 2300, 1000, 3500, 2400,
  1700, 2500, 3000, 1000, 2000, 3000),
  b1 = c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  b2 = c(1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L),
  b3 = c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
  b4 = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  b5 = c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  nature = factor(rep(c(1, 2, 3, 4), 3L)),
  job = factor(c(rep(1, 4), rep(2, 4), rep(3, 4))))
save(outcomes, file = file.path(dn, "outcomes.RData"))

ratings = data.frame(x = c(2000, 1000, 4000, 3000),
  s1 = c(1, 1, 1, 1),
  s2 = c(0, 1, 1, 1),
  s3 = c(0, 0, 0, 1),
  jobs = as.factor(c(1, 2, 1, 2)),
  states = as.factor(c(1, 1, 1, 1)))
save(ratings, file = file.path(dn, "ratings.RData"))