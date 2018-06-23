#' @title Construct A Preference System
#' @description
#'   Constructs a Preference System from a Decision Problem.
#' @param dp [\code{dp}]\cr
#'   Decison Problem calculated with \code{\link{makeDecisionProblem}}.
#' @return [\code{list}] With entries:\cr
#'   R1: Preorder on the acts.\cr
#'   R2: Preorder on R1.
#' @export
makePreferenceSystem = function(dp) {
  if(!(is(dp, "DecisionProblem"))) {
    stop("Wrong parameter type: Please provide an object
      of class: 'Decision Problem'")
  }

  # get data from dp object
  ordinals = dp$ordinal.information
  cardinals = dp$cardinal.information
  n.obs = nrow(dp$df)

  # take preference set and setup R1
  # for each set compare to all other sets and determine if it's
  # a superset

  # first cardinal info
  if (is.null(cardinals)) {
    supersets.cardinals = makeAllTrueData(n.obs^2, n.obs)
  } else {
    supersets.cardinals = lapply(cardinals, function(set1) {
      set.ids = lapply(cardinals, function(set2) {
        all(set1 >= set2)
      })
      unlist(set.ids)
    })
    supersets.cardinals = do.call("rbind", supersets.cardinals)
  }

  if (is.null(ordinals)) {
    supersets.ordinals = makeAllTrueData(n.obs^2, n.obs)
  } else {
    supersets.ordinals = lapply(ordinals, function(set1) {
      set.ids = lapply(ordinals, function(set2) {
        isSuperset(set1, set2)
      })
      unlist(set.ids)
    })
    supersets.ordinals = do.call("rbind", supersets.ordinals)
  }


  # setup R1
  R1 = as.data.frame(which(supersets.cardinals & supersets.ordinals, TRUE))
  names(R1) = c("1","2")
  # # rm duplicated entries
  R1 = R1[R1[, 1L] != R1[, 2L], ]
  rownames(R1) = seq_len(nrow(R1))

  # setup R2
  R2 = expand.grid.df(R1, R1)[, c(3:4, 1:2)]
  names(R2) = c("1.1", "1.2", "2.1", "2.2")
  # # rm duplicated entries
  R2 = R2[!((R2[, 1L] == R2[, 3L]) & (R2[, 2L] == R2[, 4L])), ]

  if (is.null(cardinals)) {
    diffs.cardinals = makeAllTrueData(nrow(R2), 1L)
  } else {
    diffs.cardinals = apply(R2, 1L, function(pairs) {
      # get differences and see if > 0
      all(cardinals[[pairs[1L]]] - cardinals[[pairs[2L]]] >=
      cardinals[[pairs[3L]]] - cardinals[[pairs[4L]]])
    })
  }

  if (is.null(ordinals)) {
    diffs.ordinals = makeAllTrueData(nrow(R2), 1L)
  } else {
    diffs.ordinals = apply(R2, 1L, function(pairs) {
      isSuperset(ordinals[[pairs[1L]]], ordinals[[pairs[3L]]]) &&
      isSuperset(ordinals[[pairs[4L]]], ordinals[[pairs[2L]]])
    })
  }

  # combine both requirements
  pair.in.R2 = unlist(diffs.cardinals) & unlist(diffs.ordinals)

  # setup R2 as grid from R1 and drop unnecessary entries
  R2 = subset(R2, subset = pair.in.R2)
  rownames(R2) = seq_len(nrow(R2))

  # then duplicated entries
  res = makeS3Obj("PreferenceSystem", df = dp$df, R1 = R1, R2 = R2,
    n.R1 = nrow(R1), n.R2 = nrow(R2))

  return(res)
}

#' @export
print.PreferenceSystem = function(x, ...) {
  cat("\nPreference System:\n")
  catf("Observations in R1: %s", x$n.R1)
  catf("Observations in R2: %s", x$n.R2)
}
