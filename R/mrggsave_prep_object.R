##' Prepare any object for use with mrggsave
##'
##' @param x a plot object
##' @param ... not currently used
##'
##' @export
mrggsave_prep_object <- function(x,...) {
  UseMethod("mrggsave_prep_object")
}
##' @export
mrggsave_prep_object.gg <- function(x,...) {
  NextMethod()
}
##' @export
mrggsave_prep_object.default <- function(x,...) {
  return(gridExtra::arrangeGrob(x))
}
##' @export
mrggsave_prep_object.ggplot <- function(x,...) {
  return(ggplotGrob(x))
}
##' @export
mrggsave_prep_object.ggmatrix <- function(x,...) {
  assert_that(requireNamespace("GGally"))
  assert_that(requireNamespace("gtable"))
  return(GGally::ggmatrix_gtable(x))
}
##' @export
mrggsave_prep_object.ggassemble <- function(x,...) {
  assert_that(requireNamespace("patchwork"))
  return(patchwork::patchworkGrob(x))
}
##' @export
mrggsave_prep_object.gtable <- function(x,...) {
  return(x)
}
##' @export
mrggsave_prep_object.arrangelist <- function(x,...) {
  return(x)
}
