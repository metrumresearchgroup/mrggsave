#' Capture a base plot
#'
#' @examples
#'
#' \dontrun{
#' x <- rnorm(100)
#' y <- rnorm(100)
#'
#' plot(x,y)
#'
#' p <- capture_base_plot()
#'
#' mrggsave(p, script = "foo.R", stem = "base_plot",dir = tempdir())
#' }
#'
#'
capture_base_plot <- function() {
  gridGraphics::grid.echo()
  grid::gList(grid::grid.grab())
}
