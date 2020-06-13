library(testthat)
library(mrggsave)

testthat::context("capture_base_plot")

test_that("capture base plot", {
  x <- rnorm(100)
  y <- rnorm(100)
  plot(x,y)
  p <- capture_base_plot()
  ans<- mrggsave(p, script = "foo.R", stem = "base_plot",dir = tempdir())
  expect_is(ans,"character")
  expect_true(file.exists(ans))
})

