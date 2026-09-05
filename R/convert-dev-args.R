convert_to_CairoPDF <- function(args) {
  require_Cairo()
  defaults <- list(
    author = "mrggsave",
    subject = "",
    creator = "",
    keywords = "",
    title = "",
    create.date = "D:19600101",
    modify.date = "D:19600101"
  )
  modifyList(defaults, args)
}
