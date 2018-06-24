#' @title Plot Hasse-Graph from a Preference System
#' @description
#'   Draws a Hasse Graph for the relation \code{R1} stored in
#'   the Preference System \code{ps}
#' @template arg_ps
#' @param ... \cr
#'   additional parameters passed down to \code{igraph::plot.igraph}
#' @return Draws Hasse Graph
#' @export
plotHasseGraph = function(ps, ...) {
  ps = checkPreferenceSystem(ps)
  r1 = ps$R1
  # create graph
  gr = igraph::graph_from_data_frame(r1, directed = TRUE, vertices = seq_len(nrow(ps$df)))
  # make layout
  root = max(table(r1[, 1L]))
  layout = igraph::layout_as_tree(gr, root = root, mode = "all")

  params = list(x = gr, layout = layout, ...)
  do.call(igraph::plot.igraph, params)
}
