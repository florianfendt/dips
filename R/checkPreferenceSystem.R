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
  n.df = nrow(df)
  if (is.null(df))
    stop("no data for states and actions given")
  checkFactorInData(df, "state")
  checkFactorInData(df, "action")
  # check R1
  R1 = ps$R1
  if (is.null(R1))
    stop("A Preference System needs a list entry R1")
  if (all(dim(R1) == rep(n.df, 2L))) {
    if(!all(unlist(R1) %in% c(0,1) | unlist(R1) %in% c(FALSE, TRUE)))
      stop("If a square matrix is all elements of R1 must be logical.
        E.g.: 0,1 or TRUE,FALSE")
    # if user has given matrix in n x n we transform it
    R1 = apply(ps$R1 == 1L, 1L, which)
    first.col = as.integer(unlist(Map(rep, seq_along(R1),
      unlist(lapply(R1, length)))))
    R1 = data.frame("1" = first.col, "2" = unlist(R1))
    R1 = R1[R1[, 1L] != R1[, 2L], ]
    colnames(R1) = c("1", "2")
    rownames(R1) = seq_len(nrow(R1))

    ps$R1 = R1
  } else {
    if (ncol(R1) != 2L)
      stop("R1 should have 2 columns")
    if (any(unlist(R1) > n.df))
      stop("Some entries of R1 are not defined in df.")
  }
  # check R2
  R2 = ps$R2
  if (is.null(R2))
    stop("A Preference System needs a list entry R2")
  if (all(dim(R2) == rep(n.df, 2L)^2)) {
    # if user has given matrix in n^2 x n^2 we transform it
    r2 = apply(R2 == 1L, 1L, which)
    rels = expand.grid(seq_len(n.df), seq_len(n.df))
    rels = rels[order(rels[, 1L]), ]
    R2 = Map(function(row, col) {
      cbind(matrix(rep(row, times = length(col)), ncol = 2, byrow = TRUE),
        rels[col, ])
    }, convertRowsToList(as.matrix(rels)), r2)
    R2 = rbindForLists(R2)
    R2 = R2[!((R2[, 1L] == R2[, 3L]) & (R2[, 2L] == R2[, 4L])), ]
    colnames(R2) = c("1.1", "1.2", "2.1", "2.2")
    rownames(R2) = seq_len(nrow(R2))
    ps$R2 = R2

  } else {
  if (ncol(R2) != 4L)
    stop("R2 should have 4 columns")
  if (any(unlist(R2) > nrow(df)))
    stop("Some entries of R2 are not defined in df.")
  }
  if(is.null(ps$n.R1))
    ps$n.R1 = nrow(ps$R1)
  if(is.null(ps$n.R2))
    ps$n.R2 = nrow(ps$R2)

  return(ps)
}
