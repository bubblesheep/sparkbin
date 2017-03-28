#-----------------------------------------------------------------------------#
# OPTIMAL BINNING ALGORITHMS
#-----------------------------------------------------------------------------#

# Tree-based Algorithms--------------------------------------------------------

#' Optimal binning using classification tree method
#' @param x the initial binning object, nominalbin or intervalbin classes
#' @param ... additional arguments
#' @export
#'
bin_tree <- function(x, ...) {
  UseMethod("bin_tree")
}

#' Optimal binning using tree for numeric variable
#'
#' @param x the initial binning object
#' @param ... additional parameters passed to tree function call
#' @return a new intervalbin object
#' @export
#'
bin_tree.intervalbin <- function(x, ...) {

}

#' Optimal binning using tree for character variable
#'
#' @param x the initial binning object
#' @param ... additional parameters passed to tree function call
#' @return a new nominalbin object
#' @export
#'
bin_tree.nominalbin <- function(x, ...) {

}
