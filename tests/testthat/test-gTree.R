library(testthat)
library(mrggsave)

testthat::context("gTree objects")

withr::local_options(list(mrggsave.path.type = "none"))

test_that("gTree [MRGS-TEST-027]", {
  pg <- grid::gTree(ggplot2::ggplot())
  expect_is(pg,"gTree")
  out <- mrggsave(pg, "foo.R", stem = "gTree", dir = tempdir())
  expect_equal(basename(out), "gTree.pdf")
})

