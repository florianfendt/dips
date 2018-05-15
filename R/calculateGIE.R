#' @title Construct A Preference System
#' @description
#'   Constructs a Preference System from a Decision Problem.
#' @param ps [\code{dp}]\cr
#'   Decison Problem calculated with \code{\link{makeDecisionProblem}}.
#' @return [\code{list}] With entries:\cr
#'   R1: Pre order on the acts.\cr
#'   R2: Preorder on R1.
#' @export
calculateGIE = function(ps, delta, p.measures) {

  I.R1 = getI(ps$R1)
  P.R1 = getP(ps$R1)

  I.R2 = getI(ps$R2)
  P.R2 = getP(ps$R2)


  obj.f = rep(0, times = nrow(ps$A))
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

  lp(direction = "max", obj.f, const, const.dir, rhos)
}

rbindForLists = function(x) {
  if (!is.null(x)) {
    x = do.call("rbind", x)
  }

  x
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
      # const[n.const] = 1
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
          # const[n.const] = 1
          const.dir = "<="
        } else {
          stop("wrong type")
        }
      }
    }
  }
  const = as.data.frame(t(const))
  names(const) = 1:length(const)
  res = cbind(const, data.frame(rhos = delta, const.dir = const.dir,
    stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  res
}
