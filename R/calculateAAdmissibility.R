#' @title Calculate A-Admissibility
#' @description
#'   Checks if an action is A-Admissible for a given set of
#'   probability measures.
#' @template arg_ps
#' @param action [\code{character}]\cr
#'   The act for which it should be checked if it is
#'   A-Admissible.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#' @template arg_pmeasures
#' @return [\code{numeric}]\cr
#'   Optimal value of the objective function.
#' @template references
#' @export
calculateAAdmissibility = function(ps, action, p.measures) {
  # Basic arg checks and obj init
  checkPreferenceSystem(ps)
  df = ps$df
  # sanitize action
  action = sanitizeAction(action)
  checkAction(action, df$action)
  # sanitize p.measures
  assertList(p.measures)
  checkProbabilityMeasures(p.measures, df$state)

  I.R1 = getI(ps$R1)
  P.R1 = getP(ps$R1)
  I.R2 = getI(ps$R2)
  P.R2 = getP(ps$R2)


  obj.f = c(rep(0, times = nrow(ps$df)), 1)
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

  acts.other = levels(df[, "action"])
  acts.other = acts.other[acts.other != action]
  const.states = lapply(p.measures, function(p) {
    const.p = p[df[, "state"]]
    const.state = lapply(acts.other, function(act) {
      const.p[df$action != action] = (-1) * const.p[df$action != action]
      const.p[df$action %nin% c(act, action)] = 0
      c(const.p, 0)
    })
    rbindForLists(const.state)
  })

  const.states = as.data.frame(rbindForLists(const.states))
  names(const.states) = 1:n.f
  rhos.states = rep(0, times = nrow(const.states))
  const.dir.states = rep(">=", times = nrow(const.states))


  const.add = rbind(const.I.R1, const.P.R1, const.I.R2, const.P.R2,
    stringsAsFactors = FALSE)
  const = rbind(const, const.add[, 1:n.f], const.states)
  rhos = c(rhos, const.add$rhos, rhos.states)
  const.dir = c(const.dir, const.add$const.dir, const.dir.states)

  linear.program = lp(direction = "max", obj.f, const, const.dir, rhos)

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
