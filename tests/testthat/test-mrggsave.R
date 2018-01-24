library(testthat)
library(mrggsave)
library(ggplot2)

context("test-mrggsave")

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
options(mrggsave_dir = tempdir())

pl <- lattice::xyplot(y~x, data = data)
pg <- ggplot(data, aes(x = x, y = y)) + geom_point()
pG <- GGally::ggpairs(data[,c(1,2)])

test_that("lattice plot", {
  out <- mrggsave(pl, Script, "_lattice_plot")
  expect_equal(basename(out), "test-mrggsave_lattice_plot.pdf")
})

test_that("ggplot", {
  out <- mrggsave(pg, Script, "_ggplot")
  expect_equal(basename(out), "test-mrggsave_ggplot.pdf")
})

test_that("ggpairs", {
  out <- mrggsave(pG, Script, "_ggpairs")
  expect_equal(basename(out), "test-mrggsave_ggpairs.pdf")
})

test_that("list of ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "_list")
  expect_equal(basename(out), "test-mrggsave_list.pdf")
})

test_that("arranged ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "_arranged", arrange = TRUE)
  expect_equal(basename(out), "test-mrggsave_arranged.pdf")
})

test_that("arranged lattice plots", {
  p <- list(pl,pl,pl)
  out <- mrggsave(p, Script, "_lat-arranged", arrange = TRUE)
  expect_equal(basename(out), "test-mrggsave_lat-arranged.pdf")
})

test_that("error if arranged ggmatrix", {
  p <- list(pG,pG,pG)
  expect_error(
    mrggsave(p, Script, arrange = TRUE, "_list")
  )
})

