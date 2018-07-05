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




# checkProbModel = function(prob.model) {
#   # check that consts are missing in pairs
#   d = prob.model$d
#   consts = prob.model[names(prob.model) != "d"]
#   given.consts = names(consts)
#   ins.consts = c("a.in", "b.in")
#   ins.given =  ins.consts %in% given.consts

#   if (any(ins.given)) {
#     if (!all(ins.given)) {
#       stop(sprintf("Please provide either both or none
#         of the list elements in %s", collapse(ins.consts))
#     }
#   }
#   eqs.consts = c("a.in", "b.in")
#   eqs.given = eqs.consts %in% given.consts
#   if (any(eqs.given)) {
#     if (!all(eqs.given)) {
#       stop(sprintf("Please provide either both or none
#         of the list elements in %s", collapse(eqs.consts))
#     }
#   }

#   # consts.mats = consts[given.consts %in% c("a.in", "a.eq")]
#   # consts.mats.n = unlist(lapply(consts.mats, ncol)
#   # consts.mats = list()
# }




