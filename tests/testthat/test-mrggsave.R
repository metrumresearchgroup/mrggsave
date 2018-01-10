library(testthat)
library(mrggsave)
library(ggplot2)

context("test-mrggsvae")

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
Dir <- tempdir()

pl <- lattice::xyplot(y~x, data = data)
pg <- ggplot(data, aes(x = x, y = y)) + geom_point()
pG <- GGally::ggpairs(data[,c(1,2)])

test_that("lattice plot", {
  out <- mrggsave(pl, Script, "lattice_plot", dir = Dir)
  expect_equal(basename(out), "lattice_plot.pdf")
})

test_that("ggplot", {
  out <- mrggsave(pg, Script, "ggplot", dir = Dir)
  expect_equal(basename(out), "ggplot.pdf")
})

test_that("ggpairs", {
  out <- mrggsave(pG, Script, "ggpairs", dir = Dir)
  expect_equal(basename(out), "ggpairs.pdf")
})

test_that("list of ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "list", dir = Dir)
  expect_equal(basename(out), "list.pdf")
})

test_that("arranged ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "arranged", dir = Dir,  arrange = TRUE)
  expect_equal(basename(out), "arranged.pdf")
})

test_that("arranged lattice plots", {
  p <- list(pl,pl,pl)
  out <- mrggsave(p, Script, "lat-arranged", dir = Dir, arrange = TRUE)
  expect_equal(basename(out), "lat-arranged.pdf")
})

test_that("error if arranged ggmatrix", {
  p <- list(pG,pG,pG)
  expect_error(
    mrggsave(p, Script, arrange = TRUE, "list", dir = Dir)
  )
})

