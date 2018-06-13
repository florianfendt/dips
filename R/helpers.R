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
