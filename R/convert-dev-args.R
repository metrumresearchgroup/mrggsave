# Metadata arguments the Cairo pdf back-end accepts through `...` rather than
# as formal arguments; see the PDF back-end section of ?Cairo::Cairo.  Because
# they are not formals, mrggsave_common() has to hold them aside before it
# filters `args` down to the device's formals, or they would be dropped.
CAIRO_PDF_META <- c(
  "author", "subject", "creator", "keywords", "create.date", "modify.date"
)

# Defaults for the subset of those that mrggsave pins.  `author` is
# deliberately absent: it is a formal of mrggsave_common(), so it arrives in
# `args` already carrying the user's value or getOption("mrggsave.author").
#
# The dates are blank on purpose.  Cairo expects ISO-8601 here and silently
# drops anything it can't parse, so an empty string leaves /CreationDate and
# /ModDate out of the file altogether.  That is what keeps output byte-stable
# across runs: with no date supplied, cairo stamps the file with the time it
# was written.
convert_to_CairoPDF <- function(args) {
  defaults <- list(
    subject = "",
    keywords = "",
    title = "",
    create.date = "",
    modify.date = ""
  )
  modifyList(defaults, args)
}
