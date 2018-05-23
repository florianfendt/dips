#' @title Construct A Preference System
#' @description
#'   Constructs a Preference System from a Decision Problem.
#' @param ps [\code{dp}]\cr
#'   Decison Problem calculated with \code{\link{makeDecisionProblem}}.
#' @param p.measures [\code{list}]\cr
#'   List of probability measures. Each entry must have exactly
#'   \code{n.state}.\cr
#'   Where \code{n.state} corresponds to the number
#'   of levels the variable \code{state} has in the \code{data.frame}
#'   of the object \code{ps$df}.
#' @param action [\code{character}]\cr
#'   The act that the interval is calculated for.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#' @return [\code{list}] With entries:\cr
#'   R1: Pre order on the acts.\cr
#'   R2: Preorder on R1.
#' @export
calculateAAdmissibility = function(ps, action, p.measures) {

  I.R1 = getI(ps$R1)
  P.R1 = getP(ps$R1)

  I.R2 = getI(ps$R2)
  P.R2 = getP(ps$R2)


  obj.f = c(rep(0, times = nrow(ps$A)), 1)
  n.f = length(obj.f)
  const = as.data.frame(diag(rep(1, times = length(obj.f))))
  const = rbind(const, const)
  names(const) = 1:n.f

  rhos = c(rep(1, n.f), rep(0, n.f))

  const.dir = c(rep("<=", n.f), rep(">=", n.f))

  const.I.R1 = rbindForLists(apply(I.R1, 1L, makeConstraint, n = n.f, type = 1L))
  const.P.R1 = rbindForLists(apply(P.R1, 1L, makeConstraint, n = n.f, type = 2L))
  const.I.R2 = rbindForLists(apply(I.R2, 1L, makeConstraint, n = n.f, type = 3L))
  const.P.R2 = rbindForLists(apply(P.R2, 1L, makeConstraint, n = n.f, type = 4L))

  # print(nrow(ps$A))

  const.states = vapply(p.measures, function(p) {
    const.state = p[ps$A[, "state"]]
    const.state[ps$A$action != action] = 0
    const.state
  }, numeric(nrow(ps$A)))
  # const.states = as.vector(const.states)
  print(const.states)

  acts.other = levels(ps$A[, "action"])[levels(ps$A[, "action"]) != action]

  const.states.other = lapply(acts.other, function(act) {
    one.state = vapply(p.measures, function(p) {
      const.state = p[ps$A[, "state"]]
      const.state[ps$A$action != act] = 0
      const.state
    }, numeric(nrow(ps$A)))
    # - as.vector(one.state)
    t(one.state)
  })
  print(const.states.other)

  const.states.other = rbindForLists(const.states.other)

  print(const.states.other)
  const.states.other = apply(const.states.other, 1L, function(const) {
    c(const + const.states, 0)
  })
  print(const.states.other)
  const.states.other = as.data.frame(t(const.states.other))
  print(const.states.other)
  names(const.states.other) = 1:ncol(const.states.other)
  rhos.states = rep(0, times = nrow(const.states.other))
  const.dir.states = rep(">=", times = nrow(const.states.other))


  const.add = rbind(const.I.R1, const.P.R1, const.I.R2, const.P.R2,
    stringsAsFactors = FALSE)

  print(const.states.other)
  const = rbind(const, const.add[1:n.f], const.states.other)
  rhos = c(rhos, const.add$rhos, rhos.states)
  const.dir = c(const.dir, const.add$const.dir, const.dir.states)

  linear.program = lp(direction = "max", obj.f, const,
    const.dir, rhos)
  opt.val = linear.program$objval
  if (opt.val > 0) {
    message(sprintf("Success: Act %s is A-admissible,
      The objective function is positive: %f", action, opt.val))
  }  else {
    message(sprintf("Act %s is not A-admissible,
      The objective function is negative: %f", action, opt.val))
  }
  return(opt.val)
}
