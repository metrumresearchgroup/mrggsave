is_glist <- function(x) "gList" %in% class(x)

make_stem <- function(script,tag) {
  base <- gsub("\\.(r|R|Rmd|rmd)$", "", script)
  paste0(base,"_",paste0(tag,collapse = "_"))
}


