#' @title Plot Hasse-Graph from a Preference System
#' @description
#'   Draws a Hasse Graph for the relation \code{R1} stored in
#'   the Preference System \code{ps}
#' @template arg_ps
#' @param title [\code{character(1L)}]\cr
#'   Title for the plot. Defaults to empty string, meaning no title.
#' @param ... \cr
#'   Additional parameters passed down to \code{igraph::plot.igraph}.
#'   See \code{\link[igraph]{plot.igraph}} and
#'   \code{\link[igraph]{igraph.plotting}} for details.
#' @references
#'   \insertRef{igraph}{dips}
#' @return Draws Hasse Graph
#' @export
plotHasseGraph = function(ps, title = "", ...) {
  # param checks
  ps = checkPreferenceSystem(ps)
  assertCharacter(title, len = 1L)
  # helpers
  df = ps$df
  r1 = ps$R1
  a.seq = seq_len(nrow(df))
  # create graph
  gr = igraph::graph_from_data_frame(r1, directed = TRUE, vertices = a.seq)
  # make tree layout
  # find alternatives without links first
  uncomparables = which(rownames(df) %nin% unlist(r1))
  # subsequently find sugraphs so they get their own tree
  roots = integer()
  i = 1L
  while (nrow(r1) > 0L) {
    doms.tab = table(r1[, 1L])
    root = names(doms.tab)[which(doms.tab == max(doms.tab))]
    if (i == 1L)
      root = root[1L]
    dominated = r1[r1[, 1L] %in% root, 2L]
    r1 = r1[r1[, 1L] %nin% c(root, dominated), ]
    roots = c(roots, root)
    i = i + 1L
  }
  roots = c(roots, uncomparables)
  layout = igraph::layout_as_tree(gr, root = roots, mode = "all")

  # prepare params
  params = list(x = gr, layout = layout, main = title, ...)
  # add some params to make plot nicer in default settings,
  # e.g user did not change then in (...)-params
  if (is.null(params$vertex.label)) {
    params$vertex.label = parse(text = sprintf("a[%s]", a.seq))
  }
  # if (is.null(params$vertex.label.dist)) {
  #   params$vertex.label.dist = 2
  # }
  # if (is.null(params$vertex.label.degree)) {
  #   params$vertex.label.degree = 0
  # }
  # if (is.null(params$vertex.label.cex)) {
  #   params$vertex.label.cex = 0.8
  # }
  # if (is.null(params$vertex.label.color)) {
  #   params$vertex.label.color = "black"
  # }
  # if (is.null(params$vertex.size)) {
  #   params$vertex.size = 5
  # }
  # if (is.null(params$vertex.color)) {
  #   params$vertex.color = "black"
  # }
  if (is.null(params$edge.arrow.width)) {
    params$edge.arrow.width = 0.8
  }
  if (is.null(params$edge.arrow.size)) {
    params$edge.arrow.size = 0.5
  }
  if (is.null(params$edge.color)) {
    params$edge.color = "black"
  }
  do.call(igraph::plot.igraph, params)
}
