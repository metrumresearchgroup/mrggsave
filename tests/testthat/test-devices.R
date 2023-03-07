library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

testthat::context("test-devices")

withr::local_options(list(mrggsave.path.type = "none"))

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
options(mrggsave.dir = tempdir(), mrg.script = "test.R")

pg <- ggplot(data, aes(x = x, y = y)) + geom_point()


test_that("png [MRGS-TEST-001]", {
  foo <- mrggsave(pg, stem = "foo", dev="png")
  expect_equal(basename(foo), "foo.png")
})

test_that("bmp [MRGS-TEST-002]", {
  foo <- mrggsave(pg, stem = "foo", dev="bmp")
  expect_equal(basename(foo), "foo.bmp")
})

test_that("eps [MRGS-TEST-003]", {
  foo <- mrggsave(pg, stem = "foo", dev="eps")
  expect_equal(basename(foo), "foo.eps")
})

test_that("ps [MRGS-TEST-004]", {
  foo <- mrggsave(pg, stem = "foo", dev="ps")
  expect_equal(basename(foo), "foo.ps")
})

test_that("tiff [MRGS-TEST-005]", {
  foo <- mrggsave(pg, stem = "foo", dev="tiff")
  expect_equal(basename(foo), "foo.tiff")
})

test_that("pdf [MRGS-TEST-006]", {
  foo <- mrggsave(pg, stem = "foo", dev="pdf")
  expect_equal(basename(foo), "foo.pdf")
})

test_that("jpeg [MRGS-TEST-007]", {
  foo <- mrggsave(pg, stem = "foo", dev="jpeg")
  expect_equal(basename(foo), "foo.jpeg")
})

test_that("svg [MRGS-TEST-008]", {
  foo <- mrggsave(pg, stem = "foo", dev="svg")
  expect_equal(basename(foo), "foo.svg")
})

test_that("cairo_pdf [MRGS-TEST-009]", {
  foo <- mrggsave(pg, stem = "foo", dev="cairo_pdf")
  expect_equal(basename(foo), "foo.pdf")
})

test_that("save multiple plots to single file with cairo_pdf [MRGS-TEST-010]", {
  foo <- mrggsave(list(pg,pg,pg), stem = "multi-cairo", dev = "cairo_pdf")
  expect_identical(basename(foo), "multi-cairo.pdf")
})

test_that("save to multiple devices [MRGS-TEST-011]", {
  foo <- mrggsave(list(pg,pg), stem = "multiple", dev = "pdf,png")
  expect_true(file.exists(foo[[1]]))
  expect_true(file.exists(foo[[2]]))
  expect_true(file.exists(foo[[3]]))
  foo <- basename(foo)
  expect_equal(foo, c("multiple.pdf", "multiple001.png", "multiple002.png"))
})
