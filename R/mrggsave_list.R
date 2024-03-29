#' Label and save a list of objects
#'
#' @param x passed to [mrggsave_common()]
#' @param flatten if `TRUE`, flatten if an object is a list; this
#' should be `TRUE` unless you really don't want to flatten here
#' @param ... passed to [mrggsave_common()]
#'
#' @details
#' No arrangement is done; the objects are just labeled and save.
#'
#' The objects could be ggplot objects or ggplot
#' objects that have been arranged on a page
#' with [mrggpage()].
#'
#' @md
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
#' @inheritParams mrggsave_common
#' @param ... function calls to generate plots or plot objects
#' @param add_context not used for now
#'
#' @export
named_plots <- function(..., tag = NULL, add_context = FALSE, envir = parent.frame()) {
  args <- enexprs(...)
  na <- names(args)
  if(is.null(na)) na <- rep("", length(args))
  calls <- sapply(args, as.character, simplify = FALSE)
  funs <- sapply(calls, "[[", 1)
  na <- usub(na)
  funs[na != ""] <- na[na != ""]
  if(any(duplicated(funs))) {
    funs[duplicated(funs)] <- paste0(
      funs[duplicated(funs)],
      "-",
      na[duplicated(funs)]
    )
  }
  if(any(duplicated(funs))) {
    warning("duplicated names in output")
  }
  plots <- lapply(args,eval,envir)
  if(is.character(tag)) {
    funs <- paste0(funs, .sep(), glue(tag))
  }
  names(plots) <- sanitize_filename(funs)
  cl <- "named-plots"
  if(add_context) cl <- c("named-plots", "needs-context")
  plots <- structure(plots, class = c(cl, class(plots)))
  return(invisible(plots))
}
