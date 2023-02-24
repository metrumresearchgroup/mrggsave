library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

testthat::context("test-utils")

test_that("path is formatted", {
  file <- "bar.png"
  dir <- "images/foo"
  x <- format_path(file, dir, path.type = "none")
  expect_equal(x, file)
  x <- format_path(file, dir, path.type = "raw")
  expect_equal(x, "images/foo/bar.png")
})
