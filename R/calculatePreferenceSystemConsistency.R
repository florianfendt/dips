#' @title Calculate Preference System Consistency
#' @description
#'   Calculates the granularity up to which the
#'   given Preference System is consistent.\cr
#'   Throws an error if computation fails.
#' @template arg_ps
#' @template arg_showinfo
#' @return [\code{ConsistencyResult}] With entries:\cr
#'   opt.val: Optimal value of the objective function.\cr
#'   opt.vec: Optimal found solution vector.
#' @template ref_jansen
#' @export
calculatePreferenceSystemConsistency = function(ps, show.info = TRUE) {
  assertLogical(show.info, len = 1L)
  ps = checkPreferenceSystem(ps)
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


  const.add = rbind(const.I.R1, const.P.R1, const.I.R2, const.P.R2, stringsAsFactors = FALSE)

  const = rbind(const, const.add[1:n.f])
  rhos = c(rhos, const.add$rhos)
  const.dir = c(const.dir, const.add$const.dir)

  linear.program = lp(direction = "max", obj.f, const,
    const.dir, rhos)
  opt.val = linear.program$objval
  if (show.info) {
    if (opt.val > 0) {
      message(sprintf("Success: The Preference System is consistent,
      with granularity up to %f", opt.val))
    }  else {
      message("Failure: The Preference System is not consistent.")
    }
  }
  res = makeS3Obj("ConsistencyResult", opt.val = opt.val,
    opt.vec = linear.program$solution)
  return(res)
}


#' @title Get Indifference of Set
#' @description
#'   Calculates indifference part from a \code{matrix} or a \code{data.frame.}
#' @param x [\code{matrix}|\code{data.frame}]\cr
#'   Matrix or data.frame with 2 or 4 columns to work on R1, R2 respectively.
#' @return [\code{data.frame}]
#' @export
getI = function(x) {
  if(class(x) == "matrix") {
    x = as.data.frame(x)
  }
  n.col = ncol(x)

  if (n.col == 2L) {
    mutated = x[, 2:1]
    I.ps = apply(mutated, 1L, function(y) {
      x[x[, 1L] == y[1L] & x[, 2L] == y[2L], ]
    })
  } else {
    if (n.col == 4L) {
      mutated = x[, c(3:4, 1:2)]
      I.ps = apply(mutated, 1L, function(y) {
      x[x[, 1L] == y[1L] & x[, 2L] == y[2L] & x[, 3L] == y[3L] & x[, 4L] == y[4L], ]
      })
    }
  }

  res = do.call("rbind", I.ps)
  if (is.null(res)) {
    res = data.frame()
  }
  res
}

#' @title Get Strict Part Of Set
#' @description
#'   Calculates strict part from a \code{matrix} or a \code{data.frame.}
#' @param x [\code{matrix}|\code{data.frame}]\cr
#'   Matrix or data.frame with 2 or 4 columns to work on R1, R2 respectively.
#' @return [\code{data.frame}]
#' @export
getP = function(x) {
  I.x = getI(x)
  indifferents = rownames(I.x)
  if (length(indifferents > 0L)) {
    x = x[rownames(x) %nin% indifferents, ]
  }
  x
}

#' @export
print.ConsistencyResult = function(x, ...) {
  if (x$opt.val > 0) {
    catf("PreferenceSystem is consistent with granularity
      up to: %f", x$opt.val)
  } else {
    catf("PreferenceSystem is not consistent, optimal value
      is not positive: %f", x$opt.val)
  }
}
