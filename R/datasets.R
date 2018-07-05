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
#' @template ref_jansen
#' @keywords data
#' @docType data
NULL

#' Costumer Ratings.
#'
#' Contains the data for a decision problem that was
#' used as an example in the Master Thesis that
#' was written about this package.\cr
#' Each row corresponds to one of the 6 alternatives.
#' @format A data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{earnings}{Expected gains}
#'   \item{s1}{Logical. Star 1}
#'   \item{s2}{Logical. Star 2}
#'   \item{s3}{Logical. Star 3}
#'   \item{product}{Factor with 2 levels, describing
#'   the different states of nature.}
#'   \item{packaging}{Factor with 2 levels, one for each type of packaging.}
#' }
#'
#' @name costumer.ratings
#' @keywords data
#' @docType data
NULL
