#' Job Offers.
#'
#' Contains the data for a decision problem that was
#' used as an example in the paper:\cr
#' Concepts for Decision Making under Severe
#' Uncertainty with Partial Ordinal and Partial
#' Cardinal Preferences.\cr
#' Each row corresponds to one of the 12 alternatives.
#' @format A data frame with 12 rows and 8 variables:
#' \describe{
#'   \item{salary}{Offered wages per month}
#'   \item{b1}{Logical. Additional benefit: Overtime}
#'   \item{b2}{Logical. Additional benefit: Child care}
#'   \item{b3}{Logical. Additional benefit: Training}
#'   \item{b4}{Logical. Additional benefit: Prospects}
#'   \item{b5}{Logical. Additional benefit: Flexibility}
#'   \item{nature}{Factor with 4 levels, describing
#'   the different states of nature.}
#'   \item{job}{Factor with 4 levels, one for each job.}
#' }
#'
#' @name job.offers
#' @template references
#' @keywords data
#' @docType data
job.offers = data.frame(salary = c(5000, 2700, 2300, 1000, 3500, 2400,
  1700, 2500, 3000, 1000, 2000, 3000),
  b1 = c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  b2 = c(1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L),
  b3 = c(1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
  b4 = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  b5 = c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
  nature = factor(rep(c(1, 2, 3, 4), 3L)),
  job = factor(c(rep(1, 4), rep(2, 4), rep(3, 4))))
devtools::use_data(job.offers, overwrite = TRUE)


#' Costumer Ratings.
#'
#' Contains the data for a decision problem that was
#' used as an example in the Master Thesis that
#' was written about this package.\cr
#' Each row corresponds to one of the 4 alternatives.
#' @format A data frame with 4 rows and 5 variables:
#' \describe{
#'   \item{earnings}{Expected gains}
#'   \item{s1}{Logical. Star 1}
#'   \item{s2}{Logical. Star 2}
#'   \item{s3}{Logical. Star 3}
#'   \item{state}{Factor with 2 levels, describing
#'   the different states of nature.}
#'   \item{job}{Factor with 4 levels, one for each job.}
#' }
#'
#' @name costumer.ratings
#' @keywords data
#' @docType data
costumer.ratings = data.frame(earnings = c(2000, 4000, 1000, 3000),
  s1 = c(1, 1, 1, 1),
  s2 = c(0, 1, 0, 1),
  s3 = c(0, 0, 0, 1),
  state = as.factor(c(1, 2, 1, 2)),
  job = as.factor(c(1, 1, 2, 2)))
devtools::use_data(costumer.ratings, overwrite = TRUE)
