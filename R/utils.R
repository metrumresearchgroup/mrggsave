is_glist <- function(x) "gList" %in% class(x) # nocov

make_stem <- function(script,tag) {
  base <- gsub("\\.(r|R|Rmd|rmd)$", "", script)
  paste0(base,"_",paste0(tag,collapse = "_"))
}

inherits_list <- function(x) { # nocov
  inherits(x,"list")
}

flatten_plots <- function(x) {
  rlang::flatten_if(x, inherits_list)
}

context <- function(x) {
  options(mrggsave.use.context = x)
}

clear_context <- function(x) {
  options(mrggsave.use.context = NULL)
}

