#' @title Calculate A-M-Dominancy
#' @description
#'   Checks if an action is A-M-dominant for a given set of
#'   probability measures.
#' @template arg_ps
#' @param action [\code{character}]\cr
#'   The act for which it should be checked if it is
#'   A-M-dominant.\cr
#'   Must be one of the levels of the \code{action} variable
#'   in the \code{data.frame} of the object \code{ps$df}.
#' @template arg_pmeasures
#' @return [\code{numeric()}]
#'   Vector of minimal optimal value of the objective functions as returned by
#'   \code{\link{calculateLocalAdmissibility}}.
#' @template ref_jansen
#' @export
calculateAMDominancy = function(ps, action, p.measures) {
  ps = checkPreferenceSystem(ps)
  # other arg checks are done in calculateLocalAdmissibility
  df = ps$df
  other.actions = levels(df$action)[levels(df$action) != action]
  min.opts = vnapply(other.actions, function (act.other) {
    calculateLocalAdmissibility(ps, action1 = action, action2 = act.other,
      p.measures = p.measures)$min.opt
  })
  all.min.opts.positive = all(min.opts >= 0)
  if (all.min.opts.positive) {
    message(sprintf("Success: %s, is A-M-dominant", action))
  } else {
    message(sprintf("Failed: %s, is not A-M-dominant", action))
  }
  return(min.opts)
}
