library(testthat)
library(mrggsave)

testthat::context("patchwork objects")

withr::local_options(list(mrggsave.path.type = "none"))

test_that("patchwork issue-26 [MRGS-TEST-046]", {
  skip_if_not_installed("patchwork")
  pg <- ggplot2::ggplot()
  pat <- pg+pg/(pg+pg)
  expect_is(pat,"patchwork")
  out <- mrggsave(pat, "foo.R", stem = "patchwork", dir = tempdir())
  expect_equal(basename(out), "patchwork.pdf")
  x <- mrggsave_prep_object(pat)
  expect_is(x,"gtable")
})

