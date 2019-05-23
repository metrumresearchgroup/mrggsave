library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

testthat::context("test-devices")

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
options(mrggsave_dir = tempdir(), mrg_script_name = "test.R")

pg <- ggplot(data, aes(x = x, y = y)) + geom_point()


test_that("png", {
  foo <- mrggsave(pg, stem = "foo", dev="png")
  expect_equal(basename(foo), "foo.png")
})

test_that("bmp", {
  foo <- mrggsave(pg, stem = "foo", dev="bmp")
  expect_equal(basename(foo), "foo.bmp")
})

test_that("eps", {
  foo <- mrggsave(pg, stem = "foo", dev="eps")
  expect_equal(basename(foo), "foo.eps")
})

test_that("ps", {
  foo <- mrggsave(pg, stem = "foo", dev="ps")
  expect_equal(basename(foo), "foo.ps")
})

test_that("tiff", {
  foo <- mrggsave(pg, stem = "foo", dev="tiff")
  expect_equal(basename(foo), "foo.tiff")
})

test_that("pdf", {
  foo <- mrggsave(pg, stem = "foo", dev="pdf")
  expect_equal(basename(foo), "foo.pdf")
})

test_that("jpeg", {
  foo <- mrggsave(pg, stem = "foo", dev="jpeg")
  expect_equal(basename(foo), "foo.jpeg")
})

test_that("svg", {
  foo <- mrggsave(pg, stem = "foo", dev="svg")
  expect_equal(basename(foo), "foo.svg")
})
