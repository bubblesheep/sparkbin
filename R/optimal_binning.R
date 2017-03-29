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
  # We do not need to consider Missing in this case
  # Build up a data first
  d <- with(x, {
    n <- length(good)
    data.frame(x = c(1:n, 1:n),
               y = factor(c(rep(0, n), rep(1, n))),
               wt = c(good, bad))
  })
  # Train Classification Tree and extract the new cut points
  fit <- party::ctree(y ~ x, data = d, weights = d$wt)
  extract_terminal <- function(node) {
    if (is.null(node)) return()
    if (node$terminal) return()
    cuts <<- c(cuts, node$psplit$splitpoint)
    extract_terminal(node$left)
    extract_terminal(node$right)
  }
  cuts <- c()
  extract_terminal(fit@tree)
  # Obtain new counts for good and bad
  # In the tree the range is left < x <= right
  # The number in the cuts is is corresponding to ith interval
  cuts <- sort(cuts)
  newcuts <- unique(c(cuts, length(x$good)))
  newgood <- sapply(newcuts, function(i) sum(x$good[1:i]))
  newgood <- diff(c(0, newgood))
  newbad <- sapply(newcuts, function(i) sum(x$bad[1:i]))
  newbad <- diff(c(0, newbad))
  cuts <- unique(c(-Inf, x$cuts[sort(cuts) + 1], Inf))

  newobj <- list(cuts = cuts,
                 good = newgood,
                 bad = newbad,
                 Missing = x$Missing)
  class(newobj) <- "intervalbin"
  newobj
}

#' Optimal binning using tree for character variable
#'
#' @param x the initial binning object
#' @param ... additional parameters passed to tree function call
#' @return a new nominalbin object
#' @export
#'
bin_tree.nominalbin <- function(x, ...) {
  # Let's build a weighted data frame
  d <- with(x,{
    data.frame(
      x = rep(c(xlevels, xlevels)),
      y = factor(c(rep(0, length(xlevels)), rep(1, length(xlevels)))),
      wt = c(good, bad)
    )
  })
  fit <- party::ctree(y ~ x, data = d, weights = d$wt)
  # Extract Terminal Node Splits-----------------------#
  extract_terminal <- function(node) {
    if (node$terminal) return()
    ps <- node$psplit
    if (node$left$terminal) {
      points <- levels(ps$splitpoint)[ps$splitpoint & ps$table]
      groups <<- c(groups, list(points))
    } else {
      extract_terminal(node$left)
    }
    if (node$right$terminal) {
      points <- levels(ps$splitpoint)[!(ps$splitpoint) & (ps$table)]
      groups <<- c(groups, list(points))
    } else {
      extract_terminal(node$right)
    }
  }
  groups <- list()
  extract_terminal(fit@tree)
  # Create output-- a little stupid way, can modify later---#
  ylevels <- rep(NA, length(x$xlevels))
  names(ylevels) <- x$xlevels
  for (i in seq_along(groups)) {
    ylevels[groups[[i]]] <- paste0("G", i)
  }
  names(ylevels) <- NULL
  # Output new objects
  newobj <- list(
    xlevels = x$xlevels,
    ylevels = ylevels,
    good = x$good,
    bad = x$bad,
    minp = x$minp
  )
  class(newobj) <- "nominalbin"
  newobj
}
