#' Build plot grobs against a stable set of font metrics
#'
#' Converting a plot to a grob (e.g. with [ggplot2::ggplotGrob()]) resolves
#' text sizes against whatever graphics device happens to be open at the
#' time and bakes the result into the layout as absolute units.  That makes
#' the saved file depend on the session: `RStudioGD` in the IDE versus the
#' default device under `Rscript`.  Evaluating `code` with a null `pdf()`
#' device current makes the layout - and therefore the bytes written to the
#' file - the same either way.
#'
#' @param code expression building one or more grobs.
#'
#' @md
#' @noRd
with_plot_metrics <- function(code) {
  usr_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  metrics_dev <- grDevices::dev.cur()
  on.exit({
    if(metrics_dev %in% grDevices::dev.list()) grDevices::dev.off(metrics_dev)
    if(usr_dev > 1) grDevices::dev.set(usr_dev)
  }, add = TRUE)
  force(code)
}
