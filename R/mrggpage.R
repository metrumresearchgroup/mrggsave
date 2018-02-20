##' Arrange plots on a single page
##'
##' This function is just a convenience wrapper for
##' \code{gridExtra::arrangeGrob}.
##'
##' @param x a list of plots
##' @param ncol passed to \code{gridExtra::arrangeGrob}
##' @export
mrggpage <- function(x, ..., ncol = 2) {
  if(!inherits(x,"list")) {
    x <- list(x)
  }
  x <- c(x,list(...))
  gridExtra::arrangeGrob(grobs=x, ncol = ncol)
}
