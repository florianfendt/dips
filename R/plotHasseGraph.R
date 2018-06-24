#' @title Plot Hasse-Graph from a Preference System
#' @description
#'   Draws a Hasse Graph for the relation \code{R1} stored in
#'   the Preference System \code{ps}
#' @template arg_ps
#' @param ... \cr
#'   Additional parameters passed down to \code{igraph::plot.igraph}.
#'   See \code{\link[igraph]{plot.igraph}} and
#'   \code{\link[igraph]{igraph.plotting}} for details.
#'
#' @return Draws Hasse Graph
#' @export
plotHasseGraph = function(ps, ...) {
  ps = checkPreferenceSystem(ps)
  r1 = ps$R1
  a.seq = seq_len(nrow(ps$df))
  # create graph
  gr = igraph::graph_from_data_frame(r1, directed = TRUE, vertices = a.seq)
  # make tree layout
  # find best alternatives
  doms.tab = table(r1[, 1L])
  root = which(doms.tab == max(doms.tab))
  layout = igraph::layout_as_tree(gr, root = root, mode = "all")
  # prepare params
  params = list(x = gr, layout = layout, ...)
  # add some params to make plot nicer,
  # only if user didn't change them in ...
  if (is.null(params$vertex.label)) {
    params$vertex.label = paste("a", a.seq, sep = "")
  }
  if (is.null(params$vertex.label.cex)) {
    params$vertex.label.cex = 0.8
  }
  if (is.null(params$vertex.size)) {
    params$vertex.size = 10
  }
  if (is.null(params$vertex.color)) {
    params$vertex.color = "SkyBlue2"
  }
  if (is.null(params$edge.arrow.width)) {
    params$edge.arrow.width = 0.8
  }
  if (is.null(params$edge.arrow.size)) {
    params$edge.arrow.size = 0.5
  }
  do.call(igraph::plot.igraph, params)
}
