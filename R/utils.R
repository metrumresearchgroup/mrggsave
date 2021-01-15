is_glist <- function(x) "gList" %in% class(x) # nocov

no_r_ext <- function(x) {
  gsub("\\.(r|R|Rmd|rmd)$", "", x)
}

make_stem <- function(script,tag) {
  base <- no_r_ext(script)
  paste0(base,"-",paste0(tag,collapse="-"))
}

inherits_list <- function(x) { # nocov
  inherits(x,"list")
}

flatten_plots <- function(x) {
  rlang::flatten_if(x, inherits_list)
}

context <- function(x) {
  x <- gsub(" +", "-",x)
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
    "Source graphic: ", x$source_graphic,
    if(!is.null(x$post_label)) paste0("\n",paste0(x$post_label,collapse="\n"))
  )
}

usub <- function(x) gsub(" +", "-", x)

sanitize_filename <- function(x) {
  gsub("[._-]+", "-", x)
}

frame_test <- function(arg = parent.frame()) {
  list(sys.frame(), parent.frame(), sys.frame(-1), arg = arg)
}
