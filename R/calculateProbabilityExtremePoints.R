#' @title Calculates Extreme Points for a Probability Model
#' @description
#'   Calculates Extreme points for a given Probability
#'   Model. Hereby, the model is given in form of
#'   (in-)equality constraints. See 'details'.
#' @param a1 Numerical or character matrix for
#'   inequality constraints.
#'   If vector, treated as matrix with one row.
#' @param b1 Numerical or character right
#'   hand side vector for inequality constraints
#' @param a2 Numerical or character matrix for equality constraints.
#'     If vector, treated as matrix with one row.
#' @param b2 Numerical or character right hand side vector
#'     for equality constraints.
#' @return [\code{matrix}]
#'   Where each row corresponds to one probability vector.
#' @details \code{calculateProbabilityExtremePoints} uses the \code{rcdd}
#'   package to calculate the extreme points.\cr
#'   The unit simplex is already
#'   setup internally, so only the the probabilistic constraints
#'   need to be passed to \code{calculateProbabilityExtremePoints}.
#'   See \code{\link[rcdd]{makeH}} and the
#'   \href{https://cran.r-project.org/web/packages/rcdd/vignettes/vinny.pdf}{vignette}
#'   of the \code{rcdd} package for further details.
#' @references
#'   \insertRef{rcdd}{dips}
#' @export
calculateProbabilityExtremePoints = function(a1, b1, a2, b2) {
  # set up unit simplex in H-representation
  d = ncol(a1)
  h.rep = makeH(- diag(d), rep(0, d), rep(1, d), 1)

  # add  inequality and equality constraints if given
  if (all(!(missing(a1) && missing (b1))))
    h.rep = addHin(a1, b1, h.rep)
  if (all(!(missing(a2) && missing (b2))))
    h.rep = addHeq(a2, b2, h.rep)
  # make V-representation
  v.rep = scdd(h.rep)$output
  # get extreme-points
  res = v.rep[, 3:ncol(v.rep)]
  return(res)
}
