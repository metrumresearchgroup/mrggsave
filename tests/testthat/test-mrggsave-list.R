library(testthat)
library(mrggsave)
library(ggplot2)

testthat::context("test-mrggsave-list")

withr::local_options(list(mrggsave.path.type = "none"))

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
Script <- "test-mrggsave"
options(mrggsave.dir = tempdir())

p <- ggplot(data, aes(x = x, y = y)) + geom_point()

test_that("save a list [MRGS-TEST-028]", {
  x <- mrggsave_list(list(p,p,p), script = Script, stem = "testlist")
  expect_identical(basename(x),"testlist.pdf")
})

test_that("save a list of plots with no labels gh-48", {

  out <- mrggsave(
    list(p,p),
    script = Script,
    stem = "testlist-nolabel",
    onefile = FALSE,
    labeller = NULL
  )
  out <- basename(out)
  expect_length(out,2)
  expect_equal(out[[1]], "testlist-nolabel001.pdf")
  expect_equal(out[[2]], "testlist-nolabel002.pdf")
})
