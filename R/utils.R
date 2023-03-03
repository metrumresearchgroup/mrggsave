
cvec_cs <- function (x) {
  if (is.null(x) | length(x) == 0)
    return(character(0))
  x <- unlist(strsplit(as.character(x), ",", fixed = TRUE),
              use.names = FALSE)
  x <- unlist(strsplit(x, " ", fixed = TRUE), use.names = FALSE)
  x <- x[x != ""]
  if (length(x) == 0) return(character(0))
  return(x)
}

is_glist <- function(x) "gList" %in% class(x) # nocov

no_r_ext <- function(x) {
  if(!is.character(x)) return(x)
  gsub("\\.(r|R|Rmd|rmd)$", "", x)
}

make_stem <- function(script,tag) {
  base <- gsub("\\.(r|R|Rmd|rmd)$", "", script)
  paste0(base, .sep(), paste0(tag, collapse = .sep()))
}

inherits_list <- function(x) { # nocov
  inherits(x,"list")
}

flatten_plots <- function(x) {
  rlang::flatten_if(x, inherits_list)
}

context <- function(x) {
  x <- gsub(" +", .sep(), x)
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

usub <- function(x) gsub(" +", .sep(), x)

sanitize_filename <- function(x) {
  gsub("[._-]+", .sep(), x)
}

#' Format the path to an image file
#'
#' @param file the name of the image file.
#' @param dir the directory where the image file is stored.
#' @param path.type a character string indicating how the path to the image
#' file should be formatted; use `"proj"` to have the path expressed relative
#' to the Rstudio project file; use `"none"` to format without any directory
#' information (just the image file name); use `"raw"` to print the complete
#' path to the file using `dir` as-is.
#'
#' @details
#' Note that the default value for `path.type` is `"proj"`. This requires
#' that an Rstudio project file is able to be found using
#' [rprojroot::find_root()] with the [rprojroot::is_rstudio_project] criterion.
#'
#' Once mrggsave finds a root for a given working directory, it caches the value
#' for the remainder of the R session. An error will be generated if an image is
#' attempted to be saved using `path.type="proj"` but an Rstudio project file
#' was not able to be located.
#'
#' @return
#' A string with the formatted image file path.
#'
#' @examples
#' \dontrun{
#' format_path("foo.txt", "my/path")
#' format_path("foo.txt", "my/path", path.type = "none")
#' format_path("foo.txt", "my/path", path.type = "raw")
#' }
#'
#' @md
#' @export
format_path <- function(file, dir, path.type = c("proj", "none", "raw")) {
  if (!(is.character(dir) && length(dir) == 1)) {
    stop("dir must be a character scalar")
  }

  path.type <- match.arg(path.type)

  if (path.type == "proj") {
    root <- find_cached_root()
    if (is.null(root)) {
      stop("No RStudio project root found for ", getwd())
    }

    if (!fs::path_has_parent(dir, root)) {
      stop("dir is not under root\n",
           paste("dir: ", dir, "\n"),
           paste("root:", root))
    }

    return(path(path_rel(dir, start = root), file))
  }

  if(path.type=="raw") {
    return(path(dir, file))
  }

  return(file)
}

roots <- new.env(parent = emptyenv())

find_cached_root <- function() {
  wd <- getwd()
  res <- get0(wd, envir = roots)
  if (is.null(res)) {
    res <- tryCatch(find_root(is_rstudio_project | is_r_package),
                    error = function(e) NULL)
    assign(wd, res, envir = roots)
  }
  return(res)
}
