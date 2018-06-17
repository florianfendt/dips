#' @title Check if an object is a Prefrence System
#' @description
#'   This function is more used internally to check
#'   if an object has the right structure to qualify
#'   as a Prefrence System. However, for users
#'   that want to set up their own relations R1 and R2
#'   this function can be useful to check if the
#'   object was constructed the right way.
#'   Furthermore, in case the relations \code{R1} and \code{R2}
#'   are given in form of squared matrices this function will
#'   convert them to matrices with two and four columns, respectively.
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
    stop("A Preference System needs a list entry R1")
  if (all(dim(R1) == rep(length(levels(df$action)), 2L))) {
    # if user has given matrix in n x n we transform it
    r1 = apply(ps$R1 == 1L, 1L, which)
    ps$R1 = data.frame("1" = as.integer(unlist(Map(rep, names(r1),
      lapply(r1, length)))), "2" = unlist(r1))
  } else {
    if (ncol(R1) != 2L)
      stop("R1 should have 2 columns")
    if (any(unlist(R1) > nrow(df)))
      stop("Some entries of R1 are not defined in df.")
  }
  # check R2
  R2 = ps$R2
  if (is.null(R2))
    stop("A Preference System needs a list entry R2")
  if (all(dim(R2) == rep(length(levels(df$action)), 2L)^2)) {
    # if user has given matrix in n^2 x n^2 we transform it
    r2 = apply(R2 == 1L, 1L, which)
    rels = expand.grid(c(1, 2),c(1, 2))
    rels = rels[order(rels[, 1L]), ]
    R2 = Map(function(row, col) {
      cbind(matrix(rep(row, times = length(col)), ncol = 2, byrow = TRUE),
        rels[col, ])
    }, convertRowsToList(as.matrix(rels)), r2)
    R2 = rbindForLists(R2)
    colnames(R2) = rep(levels(df$action))

  } else {
  if (ncol(R2) != 4L)
    stop("R2 should have 4 columns")
  if (any(unlist(R2) > nrow(df)))
    stop("Some entries of R2 are not defined in df.")
  }
  return(ps)
}
