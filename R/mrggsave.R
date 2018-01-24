
is_glist <- function(x) "gList" %in% class(x)

make_stem <- function(script,tag) {
  base <- gsub("\\.[rR]$", "", script)
  paste0(base,tag)
}

##' Save plot objects as .pdf file after labeling with Source graphic and
##' Source code labels
##'
##' @param x an object or list of objects of class \code{gg}
##' @param script the name of the script generating the \code{gg} objects
##' @param stem to form the name of the output \code{.pdf} file
##' @param tag if specified, stem is overwritten by pasting \code{script}
##' and \code{tag} together
##' @param dir output directory for \code{.pdf} file
##' @param prefix gets prepended to the output file path in the Source
##' graphic, label
##' @param onefile passed to \code{\link{pdf}}
##' @param fontsize for Source graphic and Source code labels
##' @param textGrob.x passed to \code{textGrob} (as \code{x})
##' @param textGrob.y passed to \code{textGrob} (as \code{y})
##' @param margin numeric vector of length 4 or 1 to set top, right,
##' bottom, left margin
##' @param unit unit to go along with margin sizes
##' @param width passed to \code{\link{pdf}}
##' @param height passed to \code{\link{pdf}}
##' @param nosave logical; if \code{TRUE}, return the labeled objects
##' @param arrange logical; if \code{TRUE}, arrange the ggplot objects on a
##' single page with \code{arrangeGrob}
##' @param labsep character separator (or newline) for Source code and
##' Source graphic labels
##' @param draw if \code{TRUE}, the image is printed but not saved
##' @param ... other arguments passed to \code{\link{pdf}} or
##' \code{arrangeGrob}
##'
##' @details
##' Methods are provided for \code{ggplot} output, \code{lattice}
##' output, and \code{ggmatrix} objects (produced by
##' \code{GGally::ggpairs}).  Either a single plot object
##' or a list of objects can be passed in.  If a list of objects
##' are passed in, the plots may be written to a single file (default)
##' or multiple files (if \code{onefile} is \code{FALSE}).
##' Alternatively, \code{ggplots} and \code{lattice plots}
##' can be arranged on a single page when
##' \code{arrange} is \code{TRUE}.  \code{ggmatrix} objects
##' cannot be arranged.  An error is generated if different
##' object types are passed in a single list.
##'
##' By default, the output file name is generated from
##' the script name and the value in \code{tag}.  For example,
##' when the script is named \code{vpc_figures} and the tag
##' is passed as \code{_by_dose_group}, the output file name
##' will be \code{vpc_figures_by_dose_group.pdf}.  Alternatively,
##' the user can specify the complete stem of the file
##' name with the \code{stem} argument.
##'
##' @return
##' The name of the output file.
##'
##' @examples
##' data(Theoph)
##' require(ggplot2)
##'
##' x <- runif(1000,10,100)
##' y <- 0.3*x + rnorm(length(x),0,20)
##' data <- data.frame(x = x, y = y)
##'
##' Script <- "example.R"
##'
##' # NOTE: see default value for dir argument, which should be appropriate
##' # for project work
##'
##' # Changing it here only for the example
##' options(mrggsave_dir = tempdir())
##'
##'
##' p1 <- ggplot(data=Theoph) +
##'   geom_line(aes(x=Time, y=conc, group=Subject))
##'
##' p2 <- ggplot(data=Theoph) +
##'   geom_line(aes(x=Time, y=conc)) +
##'   facet_wrap(~Subject)
##'
##' mrggsave(p1, Script, "_plot1")
##' mrggsave(p2, Script, "_plot2")
##'
##' mrggsave(list(p1,p2), Script, "_both_plots")
##' mrggsave(list(p1,p2), Script, "_separate_files", onefile=FALSE)
##'
##' mrggsave(p1, Script, "_different_shape", width=10, height=4)
##'
##' mrggsave(list(p1,p2), Script, "_onepage", arrange=TRUE, ncol=2)
##'
##' stopifnot(require(GGally))
##'
##' p3 <- ggpairs(data)
##'
##' mrggsave(p3, Script, "_ggally_plot")
##'
##' @export
mrggsave <- function(x, ...) {
  UseMethod("mrggsave")
}

##' @rdname mrggsave
##' @export
mrggsave.trellis <- function(x,
                             script,
                             tag = NULL,
                             stem="Rplot",
                             dir = getOption("mrggsave_dir","../deliv/figure"),
                             prefix = gsub("^\\.\\./","./",dir),
                             onefile=TRUE,arrange=FALSE,labsep = "\n",
                             fontsize = 7,
                             textGrob.x = 0.0025, textGrob.y = 0.6,
                             width = 5.5, height = 8,
                             draw = FALSE,
                             nosave = FALSE,...) {

  if(!inherits(x,"list")) x <- list(x)

  if(!is.null(tag)) {
    stem <- make_stem(script,tag)
  }

  if(arrange) {
    onefile <- TRUE
    x <- lapply(x, arrangeGrob)
    x <- gList(arrangeGrob(grobs=x,...))
  }

  n  <- length(x)

  if(!onefile) {
    pdffile <- paste0(file.path(dir,stem), "%03d.pdf")
    file <- paste0(file.path(prefix,stem),
                   sprintf("%03d", seq(n)), ".pdf")
    outfile <- sprintf(pdffile, seq(n))
  } else {
    pdffile <- paste0(file.path(dir,stem), ".pdf")
    file <- paste0(file.path(prefix,stem), ".pdf")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", seq(n))
  }

  label <- paste0("\n\n\n",
                  "Source code: ", script,
                  labsep,
                  "Source graphic: ", file)

  for(i in seq_along(x)) {
    x[[i]] <- arrangeGrob(
      x[[i]],
      bottom=textGrob(
        gp=gpar(fontsize=fontsize),
        just='left',
        y=textGrob.y,
        x=textGrob.x,
        label=label[[i]]
      )
    )
  }

  if(draw) {
    .foo <- lapply(x,grid.draw)
    return(invisible(x))
  }

  if(nosave) {
    return(invisible(x))
  }

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) {
    grid.arrange(x[[i]])
  }
  grDevices::dev.off()

  return(invisible(outfile))
}

##' @rdname mrggsave
##' @export
mrggsave.gg <- function(x,...) {
  NextMethod()
}

##' @rdname mrggsave
##' @export
mrggsave.ggplot <- function(x,
                            script,
                            tag = NULL,
                            stem = "Rplot",
                            dir = getOption("mrggsave_dir","../deliv/figure"),
                            prefix = gsub("^\\.\\./","./",dir),
                            onefile=TRUE,arrange=FALSE,labsep = "\n",
                            fontsize = 7,
                            textGrob.x = 0.01, textGrob.y = 0.6,
                            margin = c(0.1, 0.2, 0.7, 0.1),
                            unit = "cm",
                            width = 5.5, height = 8,
                            draw = FALSE,
                            nosave = FALSE,...) {

  if(length(margin)!=4) {
    if(length(margin)!=1) {
      stop("margin must be length 4 or length 1")
    }
    margin <- rep(margin,4)
  }

  if(!inherits(x,"list")) x <- list(x)

  if(!is.null(tag)) {
    stem <- make_stem(script,tag)
  }

  x <- lapply(x, function(xx) {
    xx + theme(
      plot.margin = margin(
        margin[1],margin[2],margin[3],margin[4],unit
      )
    )
  })

  if(arrange) {
    onefile <- TRUE
    x <- gList(arrangeGrob(grobs=x,...))
  }

  n  <- length(x)

  if(!onefile) {
    pdffile <- paste0(file.path(dir,stem), "%03d.pdf")
    file <- paste0(file.path(prefix,stem),
                  sprintf("%03d", seq(n)), ".pdf")
    outfile <- sprintf(pdffile, seq(n))
  } else {
    pdffile <- paste0(file.path(dir,stem), ".pdf")
    file <- paste0(file.path(prefix,stem), ".pdf")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", seq(n))
  }

  label <- paste0("\n\n\n",
                  "Source code: ", script,
                  labsep,
                  "Source graphic: ", file)

  for(i in seq_along(x)) {
    x[[i]] <- arrangeGrob(
      x[[i]],
      bottom=textGrob(
        gp=gpar(fontsize=fontsize),
        just='left',
        y=textGrob.y,
        x=textGrob.x,
        label=label[[i]]
      )
    )
  }

  if(draw) {
    if(is_glist(x)) {
      grid.draw(x)
    } else {
      .foo <- lapply(x,grid.draw)
    }
    return(invisible(x))
  }

  if(nosave) {
    return(invisible(x))
  }

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) {
    grid.arrange(x[[i]])
  }
  grDevices::dev.off()

  return(invisible(outfile))
}


##' @rdname mrggsave
##' @export
mrggsave.ggmatrix <- function(x,
                              script,
                              tag = NULL,
                              stem = "Rplot",
                              dir = getOption("mrggsave_dir","../deliv/figure"),
                              prefix = gsub("^\\.\\./","./",dir),
                              onefile = TRUE, arrange = FALSE,
                              labsep = "\n",
                              fontsize = 7,
                              textGrob.x = 0.01, textGrob.y = 0.6,
                              margin = c(0.1, 0.2, 0.7, 0.1),
                              unit = "cm",
                              width = 5.5, height = 8,
                              draw = FALSE,
                              nosave = FALSE, ...) {

  if(arrange) {
    stop("Cannot arrange ggmatrix objects", call. = FALSE)
  }

  if(!is.null(tag)) {
    stem <- make_stem(script,tag)
  }

  if(length(margin)!=4) {
    if(length(margin)!=1) {
      stop("margin must be length 4 or length 1")
    }
    margin <- rep(margin,4)
  }

  if(!requireNamespace("GGally")) {
    stop("could not load GGally package", call.=FALSE)
  }
  if(!requireNamespace("gtable")) {
    stop("could not load gtable package", call.=FALSE)
  }

  if(!inherits(x,"list")) {
    x <- list(x)
  }

  x <- lapply(x, GGally::ggmatrix_gtable)

  x <- lapply(x, gtable::gtable_add_padding,
              padding = unit(margin,unit))

  n  <- length(x)

  if(!onefile) {
    pdffile <- paste0(file.path(dir,stem), "%03d.pdf")
    file <- paste0(file.path(prefix,stem),
                  sprintf("%03d", seq(n)), ".pdf")
    outfile <- sprintf(pdffile, seq(n))
  } else {
    pdffile <- paste0(file.path(dir,stem), ".pdf")
    file <- paste0(file.path(prefix,stem), ".pdf")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", seq(n))
  }

  label <- paste0("\n\n\n",
                  "Source code: ", script,
                  labsep,
                  "Source graphic: ", file)

  for(i in seq_along(x)) {
    x[[i]] <- arrangeGrob(
      x[[i]],
      bottom = textGrob(
        gp = gpar(fontsize=fontsize),
        just='left',
        y=textGrob.y,
        x=textGrob.x,
        label=label[[i]]
      )
    )
  }

  if(draw) {
    if(is_glist(x)) {
      grid.draw(x)
    } else {
      .foo <- lapply(x,grid.draw)
    }
    return(invisible(x))
  }

  if(nosave) {
    return(invisible(x))
  }

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) {
    grid.arrange(x[[i]])
  }
  dev.off()

  return(invisible(outfile))
}

##' @rdname mrggsave
##' @export
mrggsave.list <- function(x, ..., arrange = FALSE) {

  cl <- lapply(x, class)
  cl <- unlist(lapply(cl, paste, collapse = "-"), use.names=FALSE)

  if(arrange) {
    if(!all(cl==cl[1])) {
      stop("All objects are not of the same class", call. = FALSE)
    }
  } else {
    if(any(cl=="trellis")) {
      cl <- "trellis"
    }
  }

  cl <- cl[1]

  if(identical(cl, "gg-ggplot")) {
    return(mrggsave.ggplot(x, arrange = arrange,...))
  }

  if(identical(cl, "trellis")) {
    return(mrggsave.trellis(x, arrange = arrange,...))
  }

  if(identical(cl, "gg-ggmatrix")) {
    return(mrggsave.ggmatrix(x, arrange = arrange, ...))
  }

  stop("Invalid object of class: ", cl, call. = FALSE)
}

##' @export
##' @rdname mrggsave
mrggdraw <- function(..., nosave = TRUE) {
  mrggsave(..., draw = TRUE, nosave = nosave)
}

