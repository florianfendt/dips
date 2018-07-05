library(devtools)

job.offers = data.frame(salary = c(5000, 2700, 2300, 1000, 3500, 2400,
  1700, 2500, 3000, 1000, 2000, 3000),
  b1 = factor(c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)),
  b2 = factor(c(1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L)),
  b3 = factor(c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L)),
  b4 = factor(c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L)),
  b5 = factor(c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L)),
  nature = factor(rep(c(1, 2, 3, 4), 3L)),
  job = factor(c(rep(1, 4), rep(2, 4), rep(3, 4))))
use_data(job.offers, overwrite = TRUE)

costumer.ratings = data.frame(earnings = c(2000, 4000, 4000,1000, 3000, 4000),
  s1 = factor(c(1, 1, 1, 1, 1, 1), levels = c("0", "1")),
  s2 = factor(c(0, 1, 1, 0, 1, 1)),
  s3 = factor(c(0, 0, 0, 0, 1, 1)),
  product = as.factor(c("bad", "good", "very good", "bad", "good", "very good")),
  packaging = as.factor(c("budget", "budget", "budget", "premium", "premium",
    "premium")))
use_data(costumer.ratings, overwrite = TRUE)
