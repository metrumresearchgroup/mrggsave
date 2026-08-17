#' Attach an output file stem to a plot
#'
#' Use this function in a `ggplot2` pipeline (with `+`) to store the stem
#' for the output file name in the plot object itself.
#'
#' @param stem a single string to form the name of the output file.
#'
#' @details
#' The stem is saved in the plot object as `mrggsave.stem` and can be
#' retrieved with `p$mrggsave.stem`. Under `ggplot2` 4.0.0 and later, this
#' is stored in the `meta` slot of the `gg` object.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + mrggstem("wt_vs_mpg")
#'
#' p$mrggsave.stem
#'
#' @seealso [mrggsave()]
#'
#' @md
#' @export
mrggstem <- function(stem) {
  assert_that(is.character(stem))
  assert_that(length(stem)==1)
  structure(list(stem = stem), class = "mrggstem")
}

#' @param object a `mrggstem` object.
#' @param plot the plot the stem is being added to.
#' @param ... not used.
#'
#' @rdname mrggstem
#' @export
ggplot_add.mrggstem <- function(object, plot, ...) {
  plot$mrggsave.stem <- object$stem
  plot
}
