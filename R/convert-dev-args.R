# CairoPDF() takes these through `...` rather than as formal arguments; see
# the PDF back-end section of ?Cairo::Cairo.
#
# The dates are blank on purpose.  Cairo expects ISO-8601 here and silently
# drops anything it can't parse, so an empty string leaves /CreationDate and
# /ModDate out of the file altogether.  That is what keeps output byte-stable
# across runs: with no date supplied, cairo stamps the file with the time it
# was written.
convert_to_CairoPDF <- function(args) {
  defaults <- list(
    author = "mrggsave",
    subject = "",
    creator = "",
    keywords = "",
    title = "",
    create.date = "",
    modify.date = ""
  )
  modifyList(defaults, args)
}
