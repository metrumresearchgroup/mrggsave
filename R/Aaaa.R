##' @importFrom grDevices pdf dev.off graphics.off postscript
##' @importFrom ggplot2 theme margin ggplotGrob last_plot
##' @importFrom gridExtra grid.arrange arrangeGrob marrangeGrob
##' @importFrom grid grid.draw gpar grid.newpage
##' @importFrom grid textGrob gList unit grid.grab
##' @importFrom gridGraphics grid.echo
##' @importFrom assertthat assert_that
##' @importFrom rlang flatten flatten_if eval_tidy enexprs
##' @importFrom glue glue
##' @importFrom stats rnorm
##' @importFrom graphics plot
##' @importFrom utils assignInMyNamespace
##'
NULL

.global <- new.env()
.global$SEP <- "-"

#' Change the output file name separagor
#'
#' @param sep a descriptor of the file separator character
#'
#' @examples
#' \dontrun{
#' mrggsave:::output_file_sep("underscore")
#' mrggsave:::output_file_sep("hyphen")
#' mrggsave:::output_file_sep()
#' }
#'
#' @md
output_file_sep <- function(sep = c("hyphen", "underscore", "dot")) {
  sep <- match.arg(sep)
  sep_char <- '-'
  if(sep=="underscore") sep_char <- "_"
  if(sep=="dot") sep_char <- "."
  message(glue("[mrggsave] output file name sep is now {sep} ({sep_char})"))
  .global$SEP <- sep_char
  return(invisible(NULL))
}
.sep <- function() .global$SEP
