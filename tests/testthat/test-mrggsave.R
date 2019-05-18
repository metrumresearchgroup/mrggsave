library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

context("test-mrggsave")

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
options(mrggsave_dir = tempdir())

pl <- lattice::xyplot(y~x, data = data)
pg <- ggplot(data, aes(x = x, y = y)) + geom_point()
pG <- GGally::ggpairs(data[,c(1,2)])
gt <- gridExtra::arrangeGrob(grobs = list(pg,pg,pg))

test_that("lattice plot", {
  out <- mrggsave(pl, Script, "lattice_plot")
  expect_equal(basename(out), "test-mrggsave_lattice_plot.pdf")
})

test_that("ggplot", {
  out <- mrggsave(pg, Script, "ggplot")
  expect_equal(basename(out), "test-mrggsave_ggplot.pdf")
})

test_that("ggpairs", {
  out <- mrggsave(pG, Script, "ggpairs")
  expect_equal(basename(out), "test-mrggsave_ggpairs.pdf")
})

test_that("list of ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "list")
  expect_equal(basename(out), "test-mrggsave_list.pdf")
})

test_that("list of ggpairs", {
  p <- list(pG,pG,pG)
  out <- mrggsave(p, Script, "list_pairs")
  expect_equal(basename(out), "test-mrggsave_list_pairs.pdf")
})

test_that("arranged ggplots", {
  p <- list(pg,pg,pg)
  out <- mrggsave(p, Script, "arranged", arrange = TRUE)
  expect_equal(basename(out), "test-mrggsave_arranged.pdf")
})

test_that("arranged lattice plots", {
  p <- list(pl,pl,pl)
  out <- mrggsave(p, Script, "lat-arranged", arrange = TRUE)
  expect_equal(basename(out), "test-mrggsave_lat-arranged.pdf")
})

test_that("gtable", {
  out <- mrggsave(gt, Script, stem = "gtable")
  expect_equal(basename(out), "gtable.pdf")
})

test_that("gList", {
  l <- list(pg,pg,pg)
  l <- lapply(l, ggplotGrob)
  gl <- do.call("gList", l)
  ans <- mrggsave(gl, Script, stem = "gList")
  expect_equal(basename(ans), "gList.pdf")
})

test_that("named list", {
  l <- list(a = pg, b = gt, c = pl)
  x <- mrggsave(l, Script, stem = "named_list", use_names=TRUE)
  x <- basename(x)
  expect_identical(x[1],"a.pdf")
  expect_identical(x[2],"b.pdf")
  expect_identical(x[3],"c.pdf")
})


test_that("mixed list", {
  l1 <- list(pg,pg,pg)
  l2 <- list(pg,pl)
  p3 <- mrggpage(l1)
  p4 <- mrggpage(l1, nrow = 1, ncol = 2, multiple = TRUE)
  plots <- list(pg,l1,l2,pG,gt,p3,p4)
  ans <- mrggsave(plots, Script, stem = "mixed")
  expect_equal(basename(ans), "mixed.pdf")
})

test_that("last plot", {
  print(pg)
  x <- mrggsave_last(script = Script,stem = "testlast")
  expect_identical(basename(x),"testlast.pdf")
})


