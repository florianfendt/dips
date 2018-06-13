#' @title Construct Decision Problem
#' @description
#'   Constructs a Decision Problem from a data frame.
#' @param df [\code{data.frame}]\cr
#'   Data from which the Decision Problem shall be derived.
#' @param state [\code{character(1)}]\cr
#'   Name of the variable assigning the states of nature.
#' @param action [\code{character(1)}]\cr
#'   Name of the variable assigning the acts to the data frame rows.
#' @param ordered [\code{logical(1)}]\cr
#'  Shall the Decision Problem be returned numerically ordered?
#'  Defaults to \code{FALSE}
#' @return [\code{DecisionProblem}] With entries:\cr
#'   df: The original data frame\cr
#'   cardinal.information: List of cardinal utility for each
#'   combination of state and action.\cr
#'   ordinal.information: List of ordinal utility for each
#'   combination of state and action.
#' @export
makeDecisionProblem = function(df, state, action, ordered = FALSE) {
  assertDataFrame(df)
  assertCharacter(state, len = 1L)
  assertCharacter(action, len = 1L)
  # get some help elements: col names, classes
  col.names = colnames(df)
  col.classes = getClasses(df)

  # sanitize state column
  checkFactorInData(df, state)
  new.name = "state"
  names(new.name) = state
  df = rename(df, new.name)
  levels(df$state) = 1:length(levels(df$state))

  # sanitize action column
  checkFactorInData(df, action)
  new.name = "action"
  names(new.name) = action
  df = rename(df, new.name)
  levels(df$action) = 1:length(levels(df$action))

  # sanitize variable numerical
  num.col = col.names[which.first(col.classes == "numeric")]
  if (length(num.col) == 0L)
    stop("'df' must at least contain one numeric variable!")

  ordinals.df = df[, names(df) %nin%  c(num.col, "state", "action")]
  pref.fac.names = colnames(ordinals.df)
  pref.fac.classes = getClasses(ordinals.df)

  # check other variables logical or 2 lev fac
  bool.vars = pref.fac.names[pref.fac.classes == "logical"]
  fac.vars = pref.fac.names[pref.fac.classes == "factor"]
  if (length(fac.vars > 0L)) {
    n.levels = viapply(as.data.frame(df[, fac.vars]), nlevels)
    if (!all(n.levels == 2L)) {
      not.ok = fac.vars[which(n.levels != 2L)]
      stopf("preference variables '%s' are factors but dont have
        two levels", collapse(not.ok))
    }
    df[, fac.vars] = as.logical(df[, fac.vars])
  }

  # fill list: df
  if (ordered) {
    df = df[order(df[, num.col], decreasing = TRUE), ]
  }

  ordinals.set = apply(ordinals.df, 1L, function(l) {
    pref.fac.names[as.logical(l)]
  })
  num.set = as.list(df[, num.col])

  res = makeS3Obj("DecisionProblem", df = df,
    ordinal.information = ordinals.set,
    cardinal.information = num.set)

  return(res)
}
