#' @title Construct Decision Problem
#' @description
#'   Constructs a Decision Problem from a data frame.
#' @param df [\code{data.frame}]\cr
#'   Data from which the Decision Problem shall be derived.
#' @param state [\code{character(1)}]\cr
#'   Name of the variable assigning the states of nature.
#' @param action [\code{character(1)}]\cr
#'   Name of the variable assigning the acts to the data frame rows.
#' @param exclude [\code{character}]\cr
#'   Variable names that should be excluded from the decision problem.
#'   Default is \code{NULL}, meaning no variable is excluded.
#' @return [\code{DecisionProblem}] With entries:\cr
#'   df: The original data frame\cr
#'   cardinal.information: List of cardinal utility for each
#'   combination of state and action.\cr
#'   ordinal.information: List of ordinal utility for each
#'   combination of state and action.
#' @export
makeDecisionProblem = function(df, state, action, exclude = NULL) {
  assertDataFrame(df)
  assertCharacter(state, len = 1L)
  assertCharacter(action, len = 1L)
  # get some help elements: col names, classes
  col.names = colnames(df)
  col.classes = getClasses(df)
  n.df = nrow(df)

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

  # handle excludes
  if (!is.null(exclude)) {
    if (!all(exclude %in% col.names)) {
      stop("Variable names in exclude must be present in 'df'")
    }
    df = df[, col.names %nin% exclude]
  }

  # sanitize variable numerical
  num.cols = col.names[which(col.classes == "numeric")]
  if (length(num.cols) == 0L) {
    num.cols = NULL
    num.set = NULL
  } else {
    if (length(num.cols) == 1L) {
      num.set = as.list(df[, num.cols])
    } else {
      num.set = split(df[, num.cols], seq_len(n.df))
    }
  }

  pref.fac.names = colnames(df)[colnames(df) %nin%  c(num.cols,
    "state", "action")]
  if (length(pref.fac.names) == 0L) {
    ordinals.set = NULL
    pref.fac.names = NULL
  } else {
    ordinals.df = as.data.frame(df[, pref.fac.names])
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
    }
    ordinals.set = apply(ordinals.df, 1L, function(l) {
      pref.fac.names[as.logical(l)]
    })
  }

  res = makeS3Obj("DecisionProblem", df = df,
    n.alternatives = n.df,
    ordinal.information = ordinals.set,
    cardinal.information = num.set,
    ordinal.vars = pref.fac.names,
    cardinal.vars = num.cols)

  return(res)
}


#' @export
print.DecisionProblem = function(x, ...) {
  df = x$df
  cat("\nDecision Problem:\n")
  catf("Numerical variables: %s", collapse(x$cardinal.vars, ", "))
  catf("Ordinal Variables: %s", collapse(x$ordinal.vars, ", "))
  catf("Number of alternatives: %s", x$n.alternatives)
  catf("Number of acts: %s", length(levels(df$action)))
  catf("Number of states: %s", length(levels(df$state)))
}
