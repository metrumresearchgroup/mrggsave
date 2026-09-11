# Converting a plot to a grob (e.g. with ggplot2::ggplotGrob()) resolves text
# sizes against whatever graphics device happens to be open at the time and
# bakes the answer into the layout as absolute units.  That makes the saved
# file depend on the session: RStudioGD in the IDE versus the device opened by
# getOption("device") under Rscript.  Building grobs with a null pdf() device
# current makes the layout - and so the bytes written to the file - the same
# either way.
#
# The device that was current on entry is recorded so that it can be restored
# on exit.

#' Open a device for measuring text
#'
#' @return a list describing the devices to be passed to
#' `close_metrics_device()`.
#'
#' @md
#' @noRd
open_metrics_device <- function() {
  usr_dev <- grDevices::dev.cur()
  grDevices::pdf(NULL)
  list(usr = usr_dev, metrics = grDevices::dev.cur())
}

#' Close a measuring device and restore the one that was in use
#'
#' @param state the value returned by `open_metrics_device()`.
#'
#' @md
#' @noRd
close_metrics_device <- function(state) {
  if(state$metrics %in% grDevices::dev.list()) grDevices::dev.off(state$metrics)
  if(state$usr > 1) grDevices::dev.set(state$usr)
  invisible(NULL)
}

#' Build plot grobs against a stable set of font metrics
#'
#' @param code expression building one or more grobs.
#'
#' @details
#' Use this at entry points that build grobs without going through the
#' [mrggsave()] generic, which opens the device itself so that it stays current
#' for method dispatch.
#'
#' @md
#' @noRd
with_plot_metrics <- function(code) {
  state <- open_metrics_device()
  on.exit(close_metrics_device(state), add = TRUE)
  force(code)
}
