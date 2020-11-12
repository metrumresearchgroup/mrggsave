library(testthat)
library(mrggsave)
library(ggplot2)
library(glue)

testthat::context("test-filename")

data <- data.frame(x = c(1,2,3), y = c(4,5,6))

p <- ggplot(data) + geom_point(aes(x,y))

options(mrggsave.dir = tempdir())

assign("runn", 1234, .GlobalEnv)

p <- ggplot(data) + geom_point(aes(x,y))

test_that("variable gets glued into stem", {
  ans <- mrggsave(p, script = "test-filename.R", stem = "save_{runn}", dir = tempdir())
  expect_equal(basename(ans), "save_1234.pdf")
})

test_that("variable gets glued into tag", {
  ans <- mrggsave(p, script = "file.R", tag = "save_{runn}")
  expect_equal(basename(ans), "file-save_1234.pdf")
})

test_that("an environment gets passed to glue", {
  env <- list(runn=4321)
  ans <- mrggsave(p, script = "file.R", tag = "save_{runn}",envir = env)
  expect_equal(basename(ans), "file-save_4321.pdf")
})

test_that("vector stem gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", stem = c("a", 101, "b"))
  expect_equal(basename(ans), "a-101-b.pdf")
})

test_that("vector tag gets collapsed", {
  ans <- mrggsave(p, script = "test-filename.R", tag = c("a", 101, "b"))
  expect_equal(basename(ans), "test-filename-a-101-b.pdf")
})

assign("p1", p, globalenv())
assign("p2", p, globalenv())
assign("p3", p, globalenv())
l <- named_plots(p1,p2,p3,tag = "bbb")
test_that("plots get named by object", {
  expect_identical(names(l), c("p1-bbb", "p2-bbb", "p3-bbb"))
  expect_length(l,3)
  cl <- sapply(l, is.ggplot)
  expect_true(all(cl))
})

