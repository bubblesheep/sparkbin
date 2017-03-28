#-----------------------------------------------------------------------------#
# BINNING TRANSFORMATION USING OPTIMAL BINNING DATA
#-----------------------------------------------------------------------------#

# Transformation Function For Numeric Binning Class----------------------------

#' Optimal binning transformation for numeric features
#'
#' @param sdf spark data frame
#' @param binobj numeric binning object with class "intervalbin"
#' @param input.col input feature name
#' @param output.col output feature name
#' @return a new spark data frame with transformed features
#' @export
bin_transform_num <- function(sdf, binobj, input.col, output.col) {

}

# Transformation Function For Character Binning Class--------------------------

#' Optimal binning transformation for character features
#'
#' @param sdf spark data frame
#' @param binobj character binning object with class "nominalbin"
#' @param input.col input feature name
#' @param output.col output feature name
#' @return a new spark data frame with transformed features
#' @export
bin_transform_char <- function(sdf, binobj, input.col, output.col) {

}
