#' @title Calculate A-Admissibility
#' @description
#'   Checks if an action is A-Admissible for a given set of
#'   probability measures.
#' @template arg_ps
#' @param p.measures [\code{list}]\cr
#'   List of probability measures. Each entry must have exactly
#'   \code{n.state} values.\cr
#'   Where \code{n.state} corresponds to the number
#'   of levels the variable \code{state} has in the \code{data.frame}
#'   of the object \code{ps$df}.
#' @param action [\code{character}]\cr
#'   The act for which it should be checked if it is
#'   A-Admissible.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#' @return [\code{numeric}]\cr
#'   Optimal value of the objective function.
#' @template references
#' @export
calculateAAdmissibility = function(ps, action, p.measures) {

  checkPreferenceSystem(ps)
  #FIXME: arg checks
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

  # print(nrow(ps$df))

  const.states = vapply(p.measures, function(p) {
    const.state = p[ps$df[, "state"]]
    const.state[ps$df$action != action] = 0
    const.state
  }, numeric(nrow(ps$df)))
  # const.states = as.vector(const.states)
  print(const.states)

  acts.other = levels(ps$df[, "action"])[levels(ps$df[, "action"]) != action]

  const.states.other = lapply(acts.other, function(act) {
    one.state = vapply(p.measures, function(p) {
      const.state = p[ps$df[, "state"]]
      const.state[ps$df$action != act] = 0
      const.state
    }, numeric(nrow(ps$df)))
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
