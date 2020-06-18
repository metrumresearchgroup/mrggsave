#' Prepare any object for use with mrggsave
#'
#' @param x a plot object
#' @param ... not currently used
#'
#' @export
mrggsave_prep_object <- function(x,...) {
  UseMethod("mrggsave_prep_object")
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.gg <- function(x,...) {
  NextMethod()
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.default <- function(x,...) {
  return(gridExtra::arrangeGrob(x))
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.ggplot <- function(x,...) {
  return(ggplotGrob(x))
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.ggmatrix <- function(x,...) {
  assert_that(requireNamespace("GGally"))
  assert_that(requireNamespace("gtable"))
  return(GGally::ggmatrix_gtable(x))
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.patchwork <- function(x,...) {
  assert_that(requireNamespace("patchwork"))
  patchwork::patchworkGrob(x)
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.gtable <- function(x,...) {
  return(x)
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.arrangelist <- function(x,...) {
  return(x)
}
#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.ggsurvplot <- function(x,...) {
  assert_that(requireNamespace("survminer"))
  survminer::arrange_ggsurvplots(list(x), nrow = 1, ncol = 1, print = FALSE)
}

#' @rdname mrggsave_prep_object
#' @export
mrggsave_prep_object.gTree <- function(x,...) {
  grid::gList(x)
}
