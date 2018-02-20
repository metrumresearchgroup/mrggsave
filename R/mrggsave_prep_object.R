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
  return(GGally::ggmatrix_gtable(x))
}
##' @export
mrggsave_prep_object.ggassemble <- function(x,...) {
  assert_that(requireNamespace("patchwork"))
  return(patchwork::patchworkGrob(x))
}
