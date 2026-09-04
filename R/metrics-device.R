# Converting a plot to a grob (e.g. with ggplot2::ggplotGrob()) resolves text
# sizes against whatever graphics device happens to be open at the time and
# bakes the answer into the layout as absolute units.  That makes the saved
# file depend on the session: RStudioGD in the IDE versus the device opened by
# getOption("device") under Rscript.  Building grobs with a null pdf() device
# current makes the layout - and so the bytes written to the file - the same
# either way.
#
# The device that was current on entry is recorded in `dev_state` so that
# draw_newpage() can still draw to it; the metrics device is invisible and
# drawing to it would just discard the plot.  Only the outermost call records
# the device, so nested calls can't overwrite it.

# dev_state environment is initialized in Aaaa.R

#' Open a device for measuring text
#'
#' @return a list describing the devices to be passed to
#' `close_metrics_device()`.
#'
#' @md
#' @noRd
open_metrics_device <- function() {
  usr_dev <- grDevices::dev.cur()
  outermost <- is.null(dev_state$usr)
  if(outermost) dev_state$usr <- usr_dev
  grDevices::pdf(NULL)
  list(usr = usr_dev, metrics = grDevices::dev.cur(), outermost = outermost)
}

#' Close a measuring device and restore the one that was in use
#'
#' @param state the value returned by `open_metrics_device()`.
#'
#' @md
#' @noRd
close_metrics_device <- function(state) {
  if(state$outermost) dev_state$usr <- NULL
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

#' Evaluate drawing code on the device the user was working on
#'
#' @param code expression drawing to the current device.
#'
#' @details
#' A no-op unless a measuring device is open; otherwise the drawing would land
#' on that (invisible) device rather than the user's.
#'
#' @md
#' @noRd
on_user_device <- function(code) {
  usr_dev <- dev_state$usr
  if(is.null(usr_dev)) return(force(code))
  metrics_dev <- grDevices::dev.cur()
  if(usr_dev == 1) {
    grDevices::dev.new() # nothing was open on entry; honors getOption("device")
    dev_state$usr <- grDevices::dev.cur()
  } else {
    grDevices::dev.set(usr_dev)
  }
  on.exit(grDevices::dev.set(metrics_dev), add = TRUE)
  force(code)
}
