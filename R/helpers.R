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

rbindForLists = function(x) {
  if (!is.null(x)) {
    x = do.call("rbind", x)
  }

  x
}


makeAllTrueData = function(n.obs, n.col) {
  as.data.frame(matrix(rep(TRUE, n.obs), ncol = n.col))
}

checkProbabilityMeasures = function(p.measures, var) {
  n.states = length(levels(var))
  p.measures.length = unique(viapply(p.measures, length))
  # check length
  if (length(p.measures.length) != 1L) {
    stop("Error for variable 'p.measures':
      Measures have different lengths")
  }
  if (p.measures.length != n.states) {
    stop(sprintf("Error for variable 'p.measures': Measures should all have length %i", n.states))
  }
  # check that elements of p.measures are true probabilities
  p.measures.sum = vnapply(p.measures, sum)
  if (any(p.measures.sum != 1)) {
    not.ok = which.first(p.measures.sum != 1)
    stop(sprintf("Error for variable 'p.measures':
      Measure  %i does not sum to 1" , not.ok))
  }
}

sanitizeAction = function(action) {
  if (testIntegerish(action, len = 1L)) {
    action = as.character(action)
  } else {
    assertCharacter(action, len = 1L)
  }
  action
}

checkAction = function(action, var) {
  all.actions = levels(var)
  # check that action is level of var
  if (action %nin% all.actions) {
    stop(sprintf("%s is not a valid action. It is not a level of the
      action variable", action))
  }
}

