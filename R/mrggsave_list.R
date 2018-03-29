##' Label and save a list of objects
##'
##' @param x passed to \code{\link{mrggsave_common}}
##' @param flatten if \code{TRUE}, flatten if an object is a list; this
##' should be \code{TRUE} unless you really don't want to flatten here
##' @param ... passed to \code{\link{mrggsave_common}}
##'
##' @details
##' No arrangement is done; the objects are just
##' labeldd and save.
##'
##' The objects could be ggplot objects or ggplot
##' objects that have been arranged on a page
##' with \code{\link{mrggpage}}.
##'
##' @export
mrggsave_list <- function(x, flatten = TRUE, ...) {

  if(!inherits(x, "list")) {
    stop("x must be a list", call. = FALSE)
  }

  if(flatten) {
    x <- flatten_plots(x)
  }

  x <- lapply(x,mrggsave_prep_object)

  mrggsave_common(x,...)
}
