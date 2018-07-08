#' @title Plots Hasse-Graph from a Preference System
#' @description
#'   Draws a Hasse Graph for the relation \code{R1} stored in
#'   the Preference System \code{ps}
#' @template arg_ps
#' @param ... \cr
#'   additional parameters passed down to \code{hasseDiagram::hasse}
#' @return Draws Hasse Graph
#' @export
plotHasseGraph = function(ps, ...) {
  ps = checkPreferenceSystem(ps)
  n.levs = nrow(ps$df)
  levs = seq_len(n.levs)
  r1 = ps$R1
  r1.mat = matrix(rep(FALSE, times = n.levs^2), nrow = n.levs, ncol = n.levs)
  for (i in levs) {
    x = as.integer(r1[i, ])
    r1.mat[x[1], x[2]] = TRUE
  }
  params = list(data = r1.mat, ...)
  do.call(hasseDiagram::hasse, params)
}
