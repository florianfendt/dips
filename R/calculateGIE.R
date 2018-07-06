#' @title Calculate Generalized Interval Expectation
#' @description
#'   Calculates the Generalized Interval Expectation for an act,
#'   on the basis of a consistent Preference System.
#' @template arg_ps
#' @param delta [\code{numeric(1L)}]\cr
#'   granularity parameter. Must be between 0 and 1
#' @template arg_pmeasures
#' @param action [\code{character}]\cr
#'   The act that the interval is calculated for.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#'
#' @return [\code{numeric(2L)}]
#'   Lower and upper bounds of the interval.
#' @template ref_jansen
#' @export
calculateGIE = function(ps, delta, p.measures, action) {

  checkPreferenceSystem(ps)
  df = ps$df
  assertNumeric(delta, len = 1L, lower = 0, upper = 1)
  # sanitize action
  action = sanitizeAction(action)
  checkAction(action, df$action)
  # sanitize p.measures
  checkProbabilityMeasures(p.measures, df$state)

  # check R1 is compact
  best.worst = viapply(1:2, function(x) {
    res = names(which(table(ps$R1[, x]) == nrow(df) - 1L))
    if (length(res) == 0L) {
      stop("There is either no best or worse alternative")
    }
    as.integer(res)
  })

  # get strict and indifference parts of R1, R2
  I.R1 = getI(ps$R1)
  P.R1 = getP(ps$R1)
  I.R2 = getI(ps$R2)
  P.R2 = getP(ps$R2)

  n.f = nrow(ps$df)
  const = as.data.frame(diag(rep(1, times = n.f)))
  const = rbind(const, const)
  names(const) = 1:n.f

  rhos = c(rep(1, n.f), rep(0, n.f))

  const.dir = c(rep("<=", n.f), rep(">=", n.f))

  const.I.R1 = rbindForLists(apply(I.R1, 1L, makeConstraintGIE, n = n.f,
    type = 1L, delta = 0))
  const.P.R1 = rbindForLists(apply(P.R1, 1L, makeConstraintGIE, n = n.f,
    type = 2L, delta = delta))
  const.I.R2 = rbindForLists(apply(I.R2, 1L, makeConstraintGIE, n = n.f,
    type = 3L, delta = 0))
  const.P.R2 = rbindForLists(apply(P.R2, 1L, makeConstraintGIE, n = n.f,
    type = 4L, delta = delta))

  const.best =  makeConstraintGIE(best.worst[1L], n.f, 5L, 1)
  const.worst =  makeConstraintGIE(best.worst[2L], n.f, 6L, 0)

  const.add = rbind(const.I.R1, const.P.R1, const.I.R2, const.P.R2, const.best, const.worst,
    stringsAsFactors = FALSE)

  const = rbind(const, const.add[1:n.f])
  rhos = c(rhos, const.add$rhos)
  const.dir = c(const.dir, const.add$const.dir)

  # get extreme points of prob.model
  opt.for.p = vapply(as.data.frame(t(p.measures)), function(p) {
    obj.f = p[ps$df[, "state"]]
    obj.f[ps$df$action != action] = 0
    min.opt = lp(direction = "min", obj.f, const,
      const.dir, rhos)$objval
    max.opt = lp(direction = "max", obj.f, const,
      const.dir, rhos)$objval
    c(min.opt, max.opt)
  }, numeric(2L))

  c(lower.bound = min(opt.for.p[1L, ]), upper.bound = max(opt.for.p[2L, ]))
}

# FIXME: Maybe refactor together with base function
makeConstraintGIE = function(indices, n, type, delta) {
  const = rep(0, n)
  n.const = n

  if (length(indices) == 0L) {
    return(NULL)
  }

  if (type == 1L) {
    const[indices] = c(1, -1)
    const.dir = "="
  } else {
    if (type == 2L) {
      const[indices] = c(-1, 1)
      const.dir = "<="
    } else {
      const2 = const
      if (type == 3L) {
        stopifnot(length(indices) == 4L)
        const2[indices[1:2]] = c(1, -1)
        const[indices[3:4]] = c(-1, 1)
        const = const + const2
        const.dir = "="
      } else {
        if (type == 4L) {
          const2[indices[1:2]] = c(-1, 1)
          const[indices[3:4]] = c(1, -1)
          const = const + const2
          const.dir = "<="
        } else {
          if (type == 5L) {
            const[indices] = 1
            const.dir = "="
            delta = -1
          } else {
            if (type == 6L){
              const[indices] = 1
              const.dir = "="
              delta = 0
            } else {
              stop("wrong type")
            }
          }
        }
      }
    }
  }
  const = as.data.frame(t(const))
  names(const) = 1:length(const)
  res = cbind(const, data.frame(rhos = -delta, const.dir = const.dir,
    stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  res
}
