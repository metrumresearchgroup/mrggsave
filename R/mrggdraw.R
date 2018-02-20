##' Draw a plot or list of plots
##'
##' The function will optionally arrange
##'
##' @param x a plot or a list of plots
##' @param ncol passed to \code{arrangeGrob}; see details
##' @param arrange logical; see details
##' @param ... passed to \code{arrangeGrob}
##'
##' @details
##' If either ncol is greater than 1 or arrange is \code{TRUE},
##' then the plot(s) will be arranged via \code{arrangeGrob}.
##'
##' @export
mrggdraw <- function(x,  ncol = 1, arrange = FALSE, ...) {
  if(!inherits(x,"list")) x <- list(x)
  x <- lapply(x,mrggsave_prep_object)
  if(ncol > 1 | (arrange)) {
    x <- arrangeGrob(grobs = x, ncol = ncol, ...)
    x <- gList(x)
  }
  foo <- lapply(x,draw_newpage)
  return(invisible(x))
}
