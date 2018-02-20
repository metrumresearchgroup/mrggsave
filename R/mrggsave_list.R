##' Label and save a list of objects
##'
##' @param x passed to \code{\link{mrggsave_common}}
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
mrggsave_list <- function(x,...) {

  if(!inherits(x, "list")) {
    stop("x must be a list", call. = FALSE)
  }

  x <- flatten_if(x, function(.x) inherits(.x,"list"))

  x <- lapply(x,mrggsave_prep_object)

  mrggsave_common(x,...)
}
