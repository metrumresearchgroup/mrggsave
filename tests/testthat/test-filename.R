library(testthat)
library(mrggsave)
library(ggplot2)
library(tibble)
library(glue)

context("test-filename")

data <- tibble(x = c(1,2,3), y = c(4,5,6))

p <- ggplot(data) + geom_point(aes(x,y))

options(mrggsave_dir = tempdir())

assign("runn", 1234, .GlobalEnv)

p <- ggplot(data) + geom_point(aes(x,y))

test_that("variable gets glued into stem", {
  ans <- mrggsave(p, script = "test-filename.R", stem = "save_{runn}", dir = tempdir())
  expect_equal(basename(ans), "save_1234.pdf")
})

test_that("variable gets glued into tag", {
  ans <- mrggsave(p, script = "file.R", tag = "save_{runn}")
  expect_equal(basename(ans), "file_save_1234.pdf")
})

test_that("an environment gets passed to glue", {
  env <- list(runn=4321)
  ans <- mrggsave(p, script = "file.R", tag = "save_{runn}",envir = env)
  expect_equal(basename(ans), "file_save_4321.pdf")
})

test_that("vector stem gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", stem = c("a", 101, "b"))
  expect_equal(basename(ans), "a_101_b.pdf")
})

test_that("vector tag gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", tag = c("a", 101, "b"))
  expect_equal(basename(ans), "test-filename_a_101_b.pdf")
})

