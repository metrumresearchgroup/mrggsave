library(testthat)
library(mrggsave)
library(ggplot2)
library(grid)

testthat::context("test-Cairo")

skip_if_not_installed("Cairo")

withr::local_options(list(mrggsave.path.type = "none"))

set.seed(1100022)
data <- data.frame(x = rnorm(100), y = rnorm(100))
options(mrggsave.dir = tempdir(), mrg.script = "test.R")

pg <- ggplot(data, aes(x = x, y = y)) + geom_point()

# ---------------------------------------------------------------------------
# Reading back what was written into the pdf.
#
# Cairo puts the document information dictionary into a compressed object
# stream, so it can't be found by scanning the file as-is.  `pdfinfo`, from
# poppler, decodes it; tests that need it are skipped where it isn't
# installed.
# ---------------------------------------------------------------------------

skip_no_pdfinfo <- function() {
  skip_if_not(nzchar(Sys.which("pdfinfo")), "pdfinfo (poppler) is not installed")
}

# Named list of the fields pdfinfo reports.  Fields absent from the document
# are absent from the list, so a pdf carrying no /CreationDate reads back as
# NULL; one carrying an empty /CreationDate reads back as "".
pdf_info <- function(file) {
  out <- system2("pdfinfo", shQuote(file), stdout = TRUE, stderr = FALSE)
  out <- out[grepl(":", out, fixed = TRUE)]
  # values can themselves contain a colon, so split on the first one only
  key <- sub(":.*$", "", out)
  val <- trimws(sub("^[^:]*:", "", out))
  stats::setNames(as.list(val), key)
}

# Save `...` under a fixed stem in a fresh directory, so that two saves of the
# same object differ only in where they were written.  mrggsave stamps the
# output file name onto the plot, so the stem has to be held constant for the
# bytes to be comparable.
save_in_new_dir <- function(x, stem, ...) {
  dir <- tempfile("mrggsave-cairo-")
  dir.create(dir)
  mrggsave(x, stem = stem, dir = dir, ...)
}

# ---------------------------------------------------------------------------
# File naming and paging
# ---------------------------------------------------------------------------

test_that("CairoPDF writes a pdf file", {
  foo <- mrggsave(pg, stem = "cairo-single", dev = "CairoPDF")
  expect_equal(basename(foo), "cairo-single.pdf")
  expect_true(file.exists(foo))
})

test_that("save multiple plots to a single file with CairoPDF", {
  foo <- mrggsave(list(pg, pg, pg), stem = "cairo-multi", dev = "CairoPDF")
  expect_identical(basename(foo), "cairo-multi.pdf")
  expect_true(file.exists(foo))
})

test_that("CairoPDF writes one page per plot", {
  skip_no_pdfinfo()

  one <- mrggsave(pg, stem = "cairo-page1", dev = "CairoPDF")
  info1 <- pdf_info(one)
  expect_identical(info1[["Pages"]], "1")

  three <- mrggsave(list(pg, pg, pg), stem = "cairo-page3", dev = "CairoPDF")
  info3 <- pdf_info(three)
  expect_identical(info3[["Pages"]], "3")
})

test_that("CairoPDF can be combined with other devices", {
  foo <- mrggsave(pg, stem = "cairo-multidev", dev = "CairoPDF,png")
  expect_equal(basename(foo), c("cairo-multidev.pdf", "cairo-multidev.png"))
  expect_true(all(file.exists(foo)))
})

test_that("CairoPDF backend requires onefile", {
  # Unlike pdf() and cairo_pdf(), the Cairo pdf backend cannot write one file
  # per page.  onefile is forced to TRUE for a single plot, so this only bites
  # when more than one plot is saved.
  expect_error(
    mrggsave(list(pg, pg), stem = "cairo-nofile", dev = "CairoPDF",
             onefile = FALSE),
    "onefile=TRUE only"
  )
})

# ---------------------------------------------------------------------------
# Document metadata
# ---------------------------------------------------------------------------

test_that("CairoPDF writes fixed document metadata", {
  skip_no_pdfinfo()
  foo <- mrggsave(pg, stem = "cairo-meta", dev = "CairoPDF")
  info <- pdf_info(foo)

  expect_equal(info$Author, "mrggsave") # getOption("mrggsave.author")
  expect_equal(info$Subject, "")
  expect_equal(info$Keywords, "")
  expect_equal(info$Creator, "")
  # title is a formal of CairoPDF(), so it keeps the device default unless the
  # caller asks for something else
  expect_equal(info$Title, "R Graphics Output")
})

test_that("CairoPDF output carries no time stamp", {
  skip_no_pdfinfo()
  # The reason for using CairoPDF at all: cairo_pdf() stamps the file with the
  # time it was written, which makes the bytes differ from run to run.
  cairo <- mrggsave(pg, stem = "cairo-nodate", dev = "CairoPDF")
  info <- pdf_info(cairo)
  expect_null(info$CreationDate)
  expect_null(info$ModDate)

  base <- mrggsave(pg, stem = "base-date", dev = "cairo_pdf")
  expect_match(pdf_info(base)$CreationDate, "[0-9]{4}") # a year
})

test_that("CairoPDF dates come from mrggsave.create/modify.date options", {
  skip_no_pdfinfo()
  # Cairo wants ISO-8601 here and silently drops anything it can't parse; the
  # default of "" is what keeps /CreationDate and /ModDate out of the file.
  foo <- withr::with_options(
    list(
      mrggsave.create.date = "2024-01-01T12:00:00",
      mrggsave.modify.date = "2025-01-01T12:00:00"
    ),
    mrggsave(pg, stem = "cairo-date-opt", dev = "CairoPDF")
  )
  info <- pdf_info(foo)
  expect_match(info$CreationDate, "2024")
  expect_match(info$ModDate, "2025")
})

test_that("a fixed date option keeps CairoPDF output reproducible", {
  op <- list(mrggsave.create.date = "2024-01-01T12:00:00")
  a <- withr::with_options(op, save_in_new_dir(pg, "repro-date", dev = "CairoPDF"))
  Sys.sleep(1.1)
  b <- withr::with_options(op, save_in_new_dir(pg, "repro-date", dev = "CairoPDF"))
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
})

test_that("the only metadata left in CairoPDF output is the cairo version", {
  skip_no_pdfinfo()
  foo <- mrggsave(pg, stem = "cairo-producer", dev = "CairoPDF")
  # /Producer is written by the cairo library itself and cannot be suppressed
  # from R, so output is only reproducible against a fixed cairo version.
  expect_match(pdf_info(foo)$Producer, "^cairo [0-9]")
})

test_that("title can be set for CairoPDF output", {
  skip_no_pdfinfo()
  foo <- mrggsave(pg, stem = "cairo-title", dev = "CairoPDF",
                  title = "Concentration vs. time")
  expect_equal(pdf_info(foo)$Title, "Concentration vs. time")
})

test_that("mrggsave.author is honored for CairoPDF output", {
  skip_no_pdfinfo()
  foo <- withr::with_options(
    list(mrggsave.author = "Metrum"),
    mrggsave(pg, stem = "cairo-author-opt", dev = "CairoPDF")
  )
  expect_equal(pdf_info(foo)$Author, "Metrum")
})

test_that("CairoPDF author cannot be set through ...", {
  skip_no_pdfinfo()
  # author is set from the option only; anything passed by the caller is
  # overwritten so that output stays under mrggsave's control
  foo <- mrggsave(pg, stem = "cairo-author", dev = "CairoPDF", author = "Kyle")
  expect_equal(pdf_info(foo)$Author, "mrggsave")
})

test_that("CairoPDF metadata passed through ... does not reach the device", {
  skip_no_pdfinfo()
  # these are pinned to "" after args is filtered down to the device formals,
  # so caller values are dropped
  foo <- mrggsave(pg, stem = "cairo-meta-args", dev = "CairoPDF",
                  subject = "PK", keywords = "conc time", creator = "my-script")
  info <- pdf_info(foo)
  expect_equal(info$Subject, "")
  expect_equal(info$Keywords, "")
  expect_equal(info$Creator, "")
})

# ---------------------------------------------------------------------------
# Reproducibility
# ---------------------------------------------------------------------------

test_that("saving the same plot twice with CairoPDF gives the same bytes", {
  a <- save_in_new_dir(pg, "repro", dev = "CairoPDF")
  Sys.sleep(1.1)
  b <- save_in_new_dir(pg, "repro", dev = "CairoPDF")
  expect_false(a == b)
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
})

test_that("CairoPDF is reproducible where cairo_pdf is not", {
  a <- save_in_new_dir(pg, "repro-base", dev = "cairo_pdf")
  Sys.sleep(1.1)
  b <- save_in_new_dir(pg, "repro-base", dev = "cairo_pdf")
  expect_false(
    identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  )
})

test_that("multi-page CairoPDF output is reproducible", {
  plots <- list(pg, pg + geom_smooth(method = "lm", formula = y ~ x))
  a <- save_in_new_dir(plots, "repro-multi", dev = "CairoPDF")
  Sys.sleep(1.1)
  b <- save_in_new_dir(plots, "repro-multi", dev = "CairoPDF")
  expect_equal(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
})

# ---------------------------------------------------------------------------
# Argument handling
# ---------------------------------------------------------------------------

test_that("device arguments still reach CairoPDF", {
  foo <- mrggsave(pg, stem = "cairo-size", dev = "CairoPDF",
                  width = 4, height = 3)
  expect_match(pdf_info(foo)$`Page size`, "^288 x 216")
})

test_that("require_Cairo passes when Cairo is installed", {
  expect_error(mrggsave:::require_Cairo(), NA)
})
