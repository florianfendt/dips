#' @title Plot Generalized Interval Expectation
#' @description
#'   Generates a plot visualizing Generalized Interval Expectation
#'   for a set of actions from a Preference System
#'   and a vector of granularities.
#' @param ps [\code{dp}]\cr
#'   Decison Problem calculated with \code{\link{makeDecisionProblem}}.
#' @param deltas [\code{numeric()}]\cr
#'   Vector of granularity parameters. Each entry must be between 0 and 1.
#' @param p.measures [\code{list}]\cr
#'   List of probability measures. Each entry must have exactly
#'   \code{n.state}.\cr
#'   Where \code{n.state} corresponds to the number
#'   of levels the variable \code{state} has in the \code{data.frame}
#'   of the object \code{ps$df}.
#' @param actions [\code{character}]\cr
#'   The acts that the interval is calculated for.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#' @return ggplot2 object.
#' @export

plotGIE = function(ps, deltas, p.measures, actions) {
  # assertNumeric(deltas, )

  interval.acts = lapply(deltas, function(delta) {
    sapply(actions, function(action) {
      calculateGIE(ps, delta, p.measures, action = action)
    })
  })

  names(interval.acts) = deltas
  interval.acts = melt(interval.acts)
  interval.acts = rename(interval.acts, c(L1 = "granularity",
    X1 = "bound", "X2" = "action"))
  interval.acts$action = as.factor(interval.acts$action)
  interval.acts$granularity = as.factor(interval.acts$granularity)
  interval.acts$granularity = factor(interval.acts$granularity,
    levels(interval.acts$granularity)[order(as.numeric(levels(interval.acts$granularity)))])

  print(str(interval.acts$granularity))
  int.plot = ggplot(interval.acts, aes_string("action", "value",
    # colour = "bound",
    group = "action"))
  int.plot = int.plot + geom_point() + geom_line()
  int.plot = int.plot + facet_wrap(~ granularity, nrow = 1L)
  int.plot = int.plot + scale_y_continuous(limits = c(0,1),
    name = "Generalized Expectation Intervals")
  int.plot = int.plot + theme_light()
  return(int.plot)
}
