#' @title Construct A Preference System
#' @description
#'   Constructs a Preference System from a Decision Problem.
#' @param dp [\code{dp}]\cr
#'   Decison Problem calculated with \code{\link{makeDecisionProblem}}.
#' @return [\code{list}] With entries:\cr
#'   R1: Pre order on the acts.\cr
#'   R2: Preorder on R1.
#' @export
makePreferenceSystem = function(dp) {
  # FIXME: check class dp

  # get data from dp object
  ordinals = dp$ordinal.information
  cardinals = dp$cardinal.information

  # take preference set and setup R1
  # for each set compare to all other sets and determine if it's
  # a superset

  # first cardinal info
  supersets.cardinals = lapply(cardinals, function(set1) {
    set.ids = lapply(cardinals, function(set2) {
      set1 >= set2
    })
    unlist(set.ids)
  })
  supersets.cardinals = do.call("rbind", supersets.cardinals)

  # setup R1 already, saves computation time later
  R1 = as.data.frame(which(supersets.cardinals, TRUE))
  R1 = R1[R1$row != R1$col, ]

  # now we can directly get the info from R1 for the ordinal case
  supersets.ordinals = apply(R1, 1L, function(pair) {
    isSuperset(ordinals[[pair[1L]]], ordinals[[pair[2L]]])
  })
  R1 = R1[supersets.ordinals, ]
  rownames(R1) = seq_len(nrow(R1))

  R2 = expand.grid.df(R1, R1)[, c(3:4, 1:2)]
  # # rm duplicated entries
  R2 = R2[!((R2$row == R2$row.1) & (R2$col == R2$col.1)), ]


  # Setup R2. First for cardinals:
  # get differences and see if > 0
  # FIXME: Make own functions for all this crab down there.
  diffs.cardinals = apply(R2, 1L, function(pairs) {
    cardinals[[pairs[1L]]] - cardinals[[pairs[2L]]] >=
    cardinals[[pairs[3L]]] - cardinals[[pairs[4L]]]
  })

  # pair.cardinals = lapply(as.list(diffs.cardinals), function(card.diff) {
  #   lapply(as.list(diffs.cardinals), function(card.diff2) {
  #     (card.diff - card.diff2) >= 0
  #   })
  # })

  # FIXME: Here the mistake probably happened!
  # Idea: Expand grid first, then: diff(1,3), diff(4, 2), then see if superset!

  # now for the ordinals, setdiff and see if diff is a superset
  diffs.ordinals = apply(R2, 1L, function(pairs) {
    isSuperset(ordinals[[pairs[1L]]], ordinals[[pairs[3L]]]) &&
    isSuperset(ordinals[[pairs[4L]]], ordinals[[pairs[2L]]])
  })

  # pair.supersets = lapply(diffs.ordinals, function(set.diff) {
  #   vlapply(diffs.ordinals, function(set.diff2) {
  #     isSuperset(set.diff, set.diff2)
  #   })
  # })

  # combine both requirements
  pair.in.R2 = unlist(diffs.cardinals) & unlist(diffs.ordinals)

  # setup R2 as grid from R1 and drop unnecessary entries
  # first uncomparables
  # R2 = expand.grid.df(R1, R1)[, c(3:4, 1:2)]
  R2 = subset(R2, subset = pair.in.R2)
  rownames(R2) = seq_len(nrow(R2))

  # then duplicated entries
  # R2 = R2[!((R2$row == R2$row.1) & (R2$col == R2$col.1)), ]


  A = dp$df
  list(A = A, R1 = R1, R2 = R2)
}
