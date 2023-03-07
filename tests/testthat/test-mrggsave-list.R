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

