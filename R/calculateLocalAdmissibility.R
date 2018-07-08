#' @title Calculate Local-Admissibility
#' @description
#'   Checks if a tupel of actions, where action1 is preferred over action2,
#'   is an element of Preference Relations, as defined in Jansen et. al 2018
#'   (see references).
#' @template arg_ps
#' @param action1 [\code{character}]\cr
#'   The act that is preferred over action2.
#' @param action2 [\code{character}]\cr
#'   The second element of the tupel, where action2 is not as desireable
#'   as action1.
#' @template arg_pmeasures
#' @template arg_keepmodels
#' @return [\code{numeric(2L)}]
#'   Optimal values of the set of linear programs in minimum and maximum form.
#' @template ref_jansen
#' @export
calculateLocalAdmissibility = function(ps, action1, action2, p.measures,
  keep.models = FALSE) {
  # Basic arg checks and obj init
  ps = checkPreferenceSystem(ps)
  df = ps$df
  n.f = nrow(ps$df)
  # sanitize actions
  action1 = sanitizeAction(action1)
  checkAction(action1, df$action)
  action2 = sanitizeAction(action2)
  checkAction(action2, df$action)
  # sanitize p.measures
  checkProbabilityMeasures(p.measures, df$state)

  # Const such that 0 <= u <= 1
  const = as.data.frame(diag(rep(1, times = n.f)))
  const = rbind(const, const)
  rhos = c(rep(1, n.f), rep(0, n.f))
  const.dir = c(rep("<=", n.f), rep(">=", n.f))

  # get strict and indiff parts of relations for const.pref
  I.R1 = getI(ps$R1)
  P.R1 = getP(ps$R1)
  I.R2 = getI(ps$R2)
  P.R2 = getP(ps$R2)
  # setup const.pref
  const.I.R1 = rbindForLists(apply(I.R1, 1L, makeConstraint, n = n.f, type = 1L,
    eps = FALSE))
  const.P.R1 = rbindForLists(apply(P.R1, 1L, makeConstraint, n = n.f, type = 2L,
    eps = FALSE))
  const.I.R2 = rbindForLists(apply(I.R2, 1L, makeConstraint, n = n.f, type = 3L,
    eps = FALSE))
  const.P.R2 = rbindForLists(apply(P.R2, 1L, makeConstraint, n = n.f, type = 4L,
    eps = FALSE))
  const.pref = rbind(const.I.R1, const.P.R1, const.I.R2, const.P.R2,
    stringsAsFactors = FALSE)
  rhos.pref = const.pref$rhos
  const.dir.pref = const.pref$const.dir
  const.pref = const.pref[ ,1:n.f]
  colnames(const) = 1:n.f

  # Bind together the different constraints
  const = rbind(const, const.pref)
  rhos = c(rhos, rhos.pref)
  const.dir = c(const.dir, const.dir.pref)

  # Do the actual optimization for every element in p.measures
  opt.for.p = lapply(as.data.frame(t(p.measures)), function(p) {
    # setup obj function
    obj.f = p[df[, "state"]]
    obj.f[df$action %nin% c(action1, action2)] = 0
    obj.f[df$action == action2] = (-1) * obj.f[df$action == action2]
    # do min and max
    min.opt = lp(direction = "min", obj.f, const, const.dir, rhos)# $objval
    max.opt = lp(direction = "max", obj.f, const, const.dir, rhos)# $objval
    list(mins = min.opt, maxs = max.opt)
  })

  minopt = min(extractSubList(opt.for.p,c ("mins", "objval")))
  maxopt = max(extractSubList(opt.for.p,c ("maxs", "objval")))

  if (minopt >= 0) {
    message(sprintf("Success: (%s, %s) is in Rel-forall", action1, action2))
  }
  if (maxopt > 0) {
    message(sprintf("Success: (%s, %s) is in Rel-exists", action1, action2))
  }
  res = list(min.opt = minopt, max.opt = maxopt)
  if (keep.models) {
    res$models = opt.for.p
  }
  return(res)
}
