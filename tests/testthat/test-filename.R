library(testthat)
library(mrggsave)
library(ggplot2)

context("test-filename")

data <- data_frame(x = c(1,2,3), y = c(4,5,6))

p <- ggplot(data) + geom_point(aes(x,y))

options(mrggsave_dir = tempdir())

test_that("vector stem gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", stem = c("a", 101, "b"))
  expect_equal(basename(ans), "a_101_b.pdf")
})

test_that("vector tag gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", tag = c("a", 101, "b"))
  expect_equal(basename(ans), "test-filename_a_101_b.pdf")
})


