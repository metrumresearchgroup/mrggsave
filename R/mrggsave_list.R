#' Label and save a list of objects
#'
#' @param x passed to \code{\link{mrggsave_common}}
#' @param flatten if \code{TRUE}, flatten if an object is a list; this
#' should be \code{TRUE} unless you really don't want to flatten here
#' @param ... passed to \code{\link{mrggsave_common}}
#'
#' @details
#' No arrangement is done; the objects are just
#' labeldd and save.
#'
#' The objects could be ggplot objects or ggplot
#' objects that have been arranged on a page
#' with \code{\link{mrggpage}}.
#'
#' @export
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

#' Generate a list of auto named plots
#'
#' @param ... function calls to generate plots or plot objects
#' @param tag used to create a name for the plot
#'
#' @export
named_plots <- function(..., tag = NULL) {
  args <- enexprs(...)
  na <- names(args)
  if(is.null(na)) na <- rep("", length(args))
  calls <- sapply(args,as.character, simplify=FALSE)
  funs <- sapply(calls, "[[",1)
  na <- usub(na)
  funs[na != ""] <- na[na != ""]
  if(any(duplicated(funs))) {
    funs[duplicated(funs)] <- paste0(funs[duplicated(funs)],"_",na[duplicated(funs)])
  }
  if(any(duplicated(funs))) {
    warning("duplicated names in output.")
  }
  plots <- lapply(args,eval,parent.frame(3))
  if(is.character(tag)) {
    funs <- paste0(funs, "_", glue(tag))
  }
  names(plots) <- funs
  return(invisible(plots))
}


