ratings = data.frame(x = c(2000, 1000, 4000, 3000),
  s1 = c(1, 1, 1, 1),
  s2 = c(0, 1, 1, 1),
  s3 = c(0, 0, 0, 1),
  jobs = as.factor(c(1, 2, 1, 2)),
  states = as.factor(c(1, 2, 1, 2)))

outcomes = data.frame(x = c(500, 270, 350, 240, 240, 100),
  b1 = c(1L, 1L, 1L, 1L, 1L, 1L),
  b2 = c(1L, 1L, 0L, 1L, 1L, 1L),
  b3 = c(1L, 0L, 0L, 0L, 0L, 1L),
  b4 = c(1L, 0L, 1L, 0L, 0L, 0L),
  nature = factor(rep(c(1, 2), 3L)),
  job = factor(c(rep(1, 2), rep(2, 2), rep(3, 2))))

outcomes.bound = outcomes
outcomes.bound[3L, c("b2", "b4")] = c(1L, 0L)
outcomes.bound[4L, "x"] = 100


act1.dom = data.frame(
  x = c(500, 270, 350, 240, 240, 100),
  b1 = c(1L, 1L, 1L, 1L, 1L, 0L),
  b2 = c(1L, 1L, 0L, 1L, 1L, 0L),
  b3 = c(1L, 0L, 0L, 0L, 0L, 0L),
  b4 = c(1L, 0L, 1L, 0L, 0L, 0L),
  nature = factor(rep(c(1, 2), 3L)),
  job = factor(c(rep(1, 2), rep(2, 2), rep(3, 2))))



act1.dom.simple = data.frame(
  x = c(500, 270, 350, 240),
  b1 = c(1L, 1L, 1L, 1L),
  b2 = c(1L, 1L, 0L, 1L),
  b3 = c(1L, 0L, 0L, 0L),
  b4 = c(1L, 0L, 1L, 0L),
  nature = factor(rep(c(1, 2), 2L)),
  job = factor(c(rep(1, 2), rep(2, 2))))


p.measures.2 = list(c(1,0), c(0.5, 0.5))
p.measures.4 = list(c(1, 0, 0, 0), c(0.5, 0.5, 0, 0),
  c(1/3, 1/3, 1/3, 0), rep(0.25, times = 4L))
