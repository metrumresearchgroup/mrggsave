##' Arrange plots on a single page
##'
##' This function is just a convenience wrapper for
##' \code{gridExtra::arrangeGrob}.
##'
##' @param x a list of plots
##' @param ... additional plot objects
##' @param ncol passed to \code{gridExtra::arrangeGrob} or
##' \code{gridExtra::marrangeGrob}
##' @param nrow passed to \code{gridExtra::arrangeGrob} or
##' \code{gridExtra::marrangeGrob}
##' @param multiple if \code{TRUE}, multiple pages will (potentially)
##' be produced with \code{gridExtra::marrangeGrob}
##' @param top passed to \code{gridExtra::marrangeGrob}
##'
##'
##' @export
mrggpage <- function(x, ..., ncol = 2, nrow = NULL, multiple = FALSE,
                     top = quote(NULL)) {
  if(!inherits(x,"list")) {
    x <- list(x)
  }
  x <- c(x,list(...))
  x <- flatten_plots(x)
  x <- lapply(x, mrggsave_prep_object)
  if(multiple) {
    if(is.null(nrow)) nrow <- 2
    gridExtra::marrangeGrob(grobs=x, ncol = ncol, nrow = nrow, top = top)
  } else {
    gridExtra::arrangeGrob(grobs=x, ncol = ncol, nrow = nrow)
  }
}
