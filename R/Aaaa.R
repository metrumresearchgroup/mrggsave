#' @importFrom grDevices pdf dev.off graphics.off postscript
#' @importFrom ggplot2 theme margin ggplotGrob last_plot
#' @importFrom gridExtra grid.arrange arrangeGrob marrangeGrob
#' @importFrom grid grid.draw gpar grid.newpage
#' @importFrom grid textGrob gList unit grid.grab
#' @importFrom gridGraphics grid.echo
#' @importFrom assertthat assert_that
#' @importFrom rlang flatten flatten_if eval_tidy enexprs is_named
#' @importFrom glue glue
#' @importFrom stats rnorm
#' @importFrom graphics plot
#' @importFrom fs path_rel path
#' @importFrom rprojroot find_root is_rstudio_project is_testthat
NULL

.global <- new.env()
.global$SEP <- "-"
.global$root <- ""
.global$has_root <- FALSE

#' Change the output file name separator
#'
#' @param sep a file separator character; this is limited to hyphen
#' (`-`), underscore (`_`) and dot (`.`)
#'
#' @examples
#' \dontrun{
#' mrggsave:::output_file_sep("_")
#' mrggsave:::output_file_sep("-")
#' mrggsave:::output_file_sep(".")
#' mrggsave:::output_file_sep()
#' }
#'
#' @md
output_file_sep <- function(sep = c("-", "_", ".")) {
  sep <- match.arg(sep)
  sep_name <- 'hyphen'
  if(sep=="_") sep_name <- "underscore"
  if(sep==".") sep_name <- "dot"
  message(glue("[mrggsave] output file name sep is now {sep_name} ({sep})"))
  .global$SEP <- sep
  return(invisible(NULL))
}
.sep <- function() .global$SEP

.onLoad <- function(libname, pkgname) {
  root <- try(
    find_root(is_rstudio_project),
    silent = TRUE
  )
  if(inherits(root, "try-error")) {
    root <- try(
      find_root(is_testthat),
      silent = TRUE
    )
    if(inherits(root, "try-error")) {
      return(NULL)
    }
  }
  .global$root <- root
  .global$has_root <- TRUE
  return(NULL)
}
