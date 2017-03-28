#-----------------------------------------------------------------------------#
# INITIAL BINNING
#-----------------------------------------------------------------------------#

# Numeric binning--------------------------------------------------------------

#' Initial binning for numeric variables
#' @param sdf Spark data frame
#' @param x the name of the numeric variable to bin
#' @param y the name of the target variable, which should take only
#'  values 0 and 1, no missing should be allowed
#' @param init_bins number of initial bins
#' @return a numeric binning object with class name "intervalbin" with the
#' following fields:
#' cuts - the break points (left end should be -Inf, right end should be Inf)
#' good - number of good (0) in each interval
#' bad - number of bad (1) in each interval
#' missing - a list with two components for missing values: good count and
#' bad count
#'
#' EXAMPLE returned object
#' obj <- list(cuts = c(-Inf, 1.5, 10, Inf),
#'             good = c(200, 300, 500),
#'             bad = c(100, 300, 900),
#'             _Missing_ = c(good = 50, bad = 50))
#' @export
#'
bin_init_num <- function(sdf, x, y, init_bins = 100) {
  NULL
}

# Character binning------------------------------------------------------------

#' Initial binning for character variables
#' @param sdf Spark data frame
#' @param x the character variable to bin
#' @param y the target variable, which should take only values 0 and 1,
#' no missing should be allowed
#' @param minp the minimum frequency of a level, if the frequency is less than
#' minp then the corresponding level will be grouped to '_Other_'
#' @return a character binning object with class name "nominalbin" with the
#' following fields:
#' xlevels - including major levels (> minp) and two special levels
#' "_Missing_" and "_Other_"
#' ylevels - mapped new levels, in the initial binning it should be same as
#' xlevels
#' good - number of good in each xlevel
#' bad - number of bad in each xlevel
#'
#' EXAMPLE of returned object
#' obj <- list(xlevels = c("A", "B", "_Missing_", "_Other_"),
#'             ylevels = c("A", "B", "_Missing_", "_Other_"),
#'             good = c(200, 200, 200, 200),
#'             bad = c(100, 200, 300, 400),
#'             minp = 0.01)
#' @export
#'
bin_init_char <- function(sdf, x, y, minp = 0.01) {
  NULL
}
