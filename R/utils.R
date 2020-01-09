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
  x <- gsub(" +", "_",x)
  options(mrggsave.use.context = x)
}

context_clear <- function(x) {
  options(mrggsave.use.context = NULL)
}

label.fun <- function(x) {
  paste0(
    x$pad,
    if(!is.null(x$pre_label)) paste0(paste0(x$pre_label,collapse="\n"),"\n"),
    "Source code: ", x$source_code,
    x$labsep,
    "Source graphic: ", x$source_file,
    if(!is.null(x$post_label)) paste0("\n",paste0(x$post_label,collapse="\n"))
  )
}

