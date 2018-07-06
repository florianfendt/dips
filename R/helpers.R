## general helpers
# wrapper for class()
getClasses = function(x) {
  res = lapply(x, class)
  unlist(res)
}
# helper function to check if var is in df and if its a factor
checkFactorInData = function(df, var) {
  if (var %nin% colnames(df))
    stopf("'df' doesn't contain '%s' variable!", var)

  if (!(is.factor(df[, var])))
    stopf("'%s' variable is not a factor!", var)
}
# simple wrapper for rbind for lists
rbindForLists = function(x) {
  if (!is.null(x)) {
    x = do.call("rbind", x)
  }

  x
}

# creates matrix with all entries set to TRUE
makeAllTrueData = function(n.obs, n.col) {
  as.data.frame(matrix(rep(TRUE, n.obs), ncol = n.col))
}



## Helpers to check sanity of function arguments

# Check that p.measures is a list of probability measures
checkProbabilityMeasures = function(p.measures, var) {
  n.states = length(levels(var))
  p.measures.length = ncol(p.measures)
  # check length
  if (p.measures.length != n.states) {
    stop(sprintf("Error for variable 'p.measures': Measures should
      have length %i", n.states))
  }
  # check that elements of p.measures are true probabilities
  p.measures.sum = apply(p.measures, 1L, sum)
  if (any(p.measures.sum != 1)) {
    not.ok = which(p.measures.sum != 1)
    stop(sprintf("Error for variable 'p.measures':
      Measure(s)  %s do not sum to 1" , collapse(not.ok)))
  }
}

# converts action to character if passed as integer, otherwise checks if action is a character
sanitizeAction = function(action) {
  if (testIntegerish(action, len = 1L)) {
    action = as.character(action)
  } else {
    assertCharacter(action, len = 1L)
  }
  action
}

# check if the action is a level of var
checkAction = function(action, var) {
  all.actions = levels(var)
  # check that action is level of var
  if (action %nin% all.actions) {
    stop(sprintf("%s is not a valid action. It is not a level of the
      action variable", action))
  }
}
