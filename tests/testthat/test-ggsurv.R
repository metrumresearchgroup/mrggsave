# library(testthat)
# library(mrggsave)
# library(ggplot2)
# library(glue)
#
# testthat::context("test-ggsurv")
#
# library(mrggsave)
# require("survival")
#
# test_that("ggsurvplot", {
#   skip_if_not(require("survminer"))
#
#   fit<- survfit(Surv(time, status) ~ sex, data = lung)
#
#   # With a new theme
#   pp1 <- ggsurvplot(fit, data = lung, ggtheme=theme_bw())
#   x <- mrggsave(pp1, script = "test-ggsurv.R", dir = tempdir(), tag = "with_theme")
#   expect_identical(basename(x),"test-ggsurv_with_theme.pdf")
#
#   # With the default theme
#   pp2 <- ggsurvplot(fit, data = lung)
#   x <- mrggsave(pp2, script = "test-ggsurv.R", dir = tempdir(), tag = "no_theme")
#   expect_identical(basename(x),"test-ggsurv_no_theme.pdf")
# })
#
#
#
