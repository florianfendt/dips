#' @title Check if an object is a Prefrence System
#' @description
#'   This function is more used internally to check
#'   if an object has the right structure to qualify
#'   as a Prefrence System. However, for users
#'   that want to set up their own relations R1 and R2
#'   this function can be useful to check if the
#'   object was constructed the right way.
#' @template arg_ps
#' @return [\code{PreferenceSystem}]\cr
#'   If checks were succesful, the input is returned.
#' @export
checkPreferenceSystem = function(ps) {
  # check df
  df = ps$df
  if (is.null(df))
    stop("no data for states and actions given")
  checkFactorInData(df, "state")
  checkFactorInData(df, "action")
  # check R1
  R1 = ps$R1
  if (is.null(R1))
    stop("A Preference System needs a list entry
      R1")
  if (ncol(R1) != 2L)
    stop("R1 should have 2 columns")
  if (any(unlist(R1) > nrow(df)))
    stop("Some entries of R1 are not defined in df.")
  # check R2
  R2 = ps$R2
  if (is.null(R2))
    stop("A Preference System needs a list entry
      R2")
  if (ncol(R2) != 4L)
    stop("R2 should have 4 columns")
  if (any(unlist(R2) > nrow(df)))
    stop("Some entries of R2 are not defined in df.")
  return(ps)
}
