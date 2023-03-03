library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

testthat::context("test-utils")

test_that("path is formatted", {
  file <- "bar.png"
  dir <- "images/foo"
  x <- format_path(file, dir, path.type = "none")
  expect_equal(x, file)
  x <- format_path(file, dir, path.type = "raw")
  expect_equal(x, "images/foo/bar.png")
})

test_that("path is formatted: path.type = proj", {
  tdir <- withr::local_tempdir()

  file <- "bar.png"
  dir <- "images/foo"

  withr::with_dir(tdir, {
    expect_error(format_path(file, dir, path.type = "proj"),
                 "No RStudio project")
  })

  dir_proj <- file.path(tdir, "project")
  fs::dir_create(dir_proj)
  cat("Version: 1.0\n", file = file.path(dir_proj, "foo.Rproj"))

  withr::with_dir(dir_proj, {
    expect_equal(format_path(file,
                             dir,
                             path.type = "proj"),
                 "images/foo/bar.png")
    expect_equal(format_path(file,
                             file.path(dir_proj, dir),
                             path.type = "proj"),
                 "images/foo/bar.png")

    expect_error(format_path(file, tdir, path.type = "proj"),
                 "not under root")
  })

  proj_subdir <- file.path(dir_proj, "subdir")
  fs::dir_create(proj_subdir)
  withr::with_dir(proj_subdir, {
    expect_equal(format_path(file,
                             file.path("..", dir),
                             path.type = "proj"),
                 "images/foo/bar.png")
    expect_equal(format_path(file,
                             file.path(dir_proj, dir),
                             path.type = "proj"),
                 "images/foo/bar.png")
  })
})
