setwd("~/PreferenceSystems")

# setwd("~/DecisionsInPreferenceSystems")

# library(roxygen2)
# library(testthat)
# library(devtools)

# roxygenize()
# check()

load_all()

outcomes = data.frame(x = c(5000, 2700, 2300, 1000, 3500, 2400,
  1700, 2500, 3000, 1000, 2000, 3000),
  b1 = c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  b2 = c(1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L),
  b3 = c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
  b4 = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  b5 = c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  nature = factor(rep(c(1, 2, 3, 4), 3L)),
  job = factor(c(rep(1, 4), rep(2, 4), rep(3, 4))))

dp.outcomes = makeDecisionProblem(outcomes,
  state = "nature", action = "job")

ps.outcomes = makePreferenceSystem(dp.outcomes)
eps = calculatePreferenceSystemConsistency(ps.outcomes)




ncol(eps.test$constraints)

ncol(eps$constraints)
eps$solution




a.mat = matrix(c(0,0,0, 1,1,1), nrow = 3)

b.mat = matrix(c(0,0,1, 1,1,1, 0,0,0), nrow = 3)



ident.test = apply(b.mat, 2L, function(x)  {
  res = apply(a.mat, 2L, function(y) {
    identical(x, y)
  })
  any(res)
})

ident.test





ident.test = apply(eps.test$constraints, 2L, function(x)  {
  res = apply(eps$constraints, 2L, function(y) {
    identical(x, y)
  })
  any(res)
})


which(!ident.test)



eps.test$constraints[,399]



ab.mat = cbind(b.mat, a.mat)


unique(ab.mat, MARGIN = 2L)




murged = cbind(eps.test$constraints, eps$constraints)


dim(unique(murged))

dim(eps$constraints)

1341 + 516




apply(ident.test, 2L, which)


merge(b.mat, a.mat)


ident.test = apply(eps.test$constraints, 2L, function(x)  {
  all(x %in% eps$constraints)
})


all(ident.test)


identical(eps.test$constraints, eps$constraints)







getI(ps.outcomes$R1)
getI(ps.outcomes$R2)
getP(ps.outcomes$R1)
getP(ps.outcomes$R2)


ex.1 = data.frame(x = c(2000, 1000, 4000, 3000),
  s1 = c(1, 1, 1, 1),
  s2 = c(0, 1, 1, 1),
  s3 = c(0, 0, 0, 1),
  packaging = as.factor(c(1, 2, 1, 2)),
  quality = as.factor(c(1, 1, 2, 2)))

ex.2 = data.frame(x = c(1000, 1000, 4000, 3000),
  s1 = c(1, 1, 1, 1),
  s2 = c(0, 0, 1, 1),
  s3 = c(0, 0, 0, 1),
  packaging = as.factor(c(1, 2, 1, 2)),
  quality = as.factor(c(1, 1, 2, 2)))


dp = makeDecisionProblem(ex.2, "quality", "packaging")
ps = makePreferenceSystem(dp)

eps1 = calculatePreferenceSystemConsistency(ps)
eps1$solution


dp2 = makeDecisionProblem(ex.2, "quality", "packaging")
ps2 = makePreferenceSystem(dp2)

# nrow(ps2$R2)
# getI(ps2$R1)

eps2 = calculatePreferenceSystemConsistency(ps2)
eps2$solution

