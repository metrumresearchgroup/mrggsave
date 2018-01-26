
is_glist <- function(x) "gList" %in% class(x)

make_stem <- function(script,tag) {
  base <- gsub("\\.(r|R|Rmd|rmd)$", "", script)
  paste0(base,tag)
}

##' Draw an annotated plot
##'
##' @param x a table grob object
##'
##' @details
##' This function calls \code{grid::grid.newpage} and
##' then \code{grid::grid.draw}.
##'
##' @export
draw_newpage <- function(x) {
  grid.newpage()
  grid.draw(x)
}

##' Label, arrange, and save graphics
##'
##' Save plot objects as .pdf file after labeling with Source graphic and
##' Source code labels.
##'
##'
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
##' @param ypad integer number of newlines to separate annotation
##' from x-axis title
##' @param width passed to \code{\link{pdf}}
##' @param height passed to \code{\link{pdf}}
##' @param .save logical; if \code{FALSE}, return the labeled objects
##' @param arrange logical; if \code{TRUE}, arrange the ggplot objects on a
##' single page with \code{arrangeGrob}
##' @param labsep character separator (or newline) for Source code and
##' Source graphic labels
##' @param draw if \code{TRUE}, the plot is drawn using \code{\link{draw_newpage}}
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
##' \code{mrggdraw} calls \code{mrggsave} and draws
##' the plot but does not save it.
##'
##' \code{mrgglabel} calls \code{mrggsave} and
##' neither draws nor saves the plot, but
##' returns the annotated plots as table grob.
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
mrggsave.ggplot <- function(x, ..., ypad = 2, arrange = FALSE, onefile = TRUE) {

  if(!inherits(x,"list")) x <- list(x)

  if(arrange) {
    onefile <- TRUE
    x <- gList(arrangeGrob(grobs=x,...))
  }

  return(mrggsave_common(x = x, ypad = ypad,
                         onefile = onefile, arrange = arrange, ...))
}


##' @rdname mrggsave
##' @export
mrggsave.ggmatrix <- function(x, ..., ypad = 4, arrange = FALSE,
                              onefile = TRUE) {

  if(arrange) {
    stop("Cannot arrange ggmatrix objects", call. = FALSE)
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

  return(mrggsave_common(x = x, ypad = ypad, arrange = FALSE, ...))
}

##' @rdname mrggsave
##' @export
mrggsave.trellis <- function(x, ..., ypad = 3, arrange = FALSE,
                             onefile = TRUE) {

  if(!inherits(x,"list")) x <- list(x)

  if(arrange) {
    onefile <- TRUE
    x <- lapply(x, arrangeGrob)
    x <- gList(arrangeGrob(grobs=x,...))
  }

  return(mrggsave_common(x = x, ypad = ypad, arrange = arrange,
                         onefile = onefile, ...))
}

##' @rdname mrggsave
mrggsave_common <- function(x,
                            script,
                            tag = NULL,
                            width = 5, height = 5,
                            stem="Rplot",
                            dir = getOption("mrggsave_dir","../deliv/figure"),
                            prefix = gsub("^\\.\\./","./",dir),
                            onefile=TRUE,
                            arrange=FALSE,
                            draw = FALSE,
                            .save = TRUE,
                            ypad = 3,
                            labsep = "\n",
                            fontsize = 7,
                            textGrob.x = 0.01,
                            textGrob.y = 0.25,
                            ...) {

  if(!is.null(tag)) {
    stem <- make_stem(script,tag)
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

  label <- paste0(paste0(rep("\n",as.integer(ypad)), collapse = ""),
                  "Source code: ", script,
                  labsep,
                  "Source graphic: ", file)

  for(i in seq_along(x)) {
    x[[i]] <- arrangeGrob(
      x[[i]],
      bottom=textGrob(
        gp=gpar(fontsize=fontsize),
        just=c('left','bottom'),
        y=textGrob.y,
        x=textGrob.x,
        label=label[[i]]
      )
    )
  }

  if(draw) {
    if(is_glist(x)) {
      draw_newpage(x)
    } else {
      .foo <- lapply(x,draw_newpage)
    }
    return(invisible(x))
  }

  if(!.save) {
    if(length(x)==1) {
      x <- x[[1]]
    }
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

##' @rdname mrggsave
##' @export
mrggsave.gg <- function(x,...) {
  NextMethod()
}

##' @export
##' @rdname mrggsave
mrggdraw <- function(..., draw = TRUE, .save = FALSE) {
  mrggsave(..., draw = TRUE, .save = .save)
}

##' @export
##' @rdname mrggsave
mrgglabel <- function(..., draw = FALSE, .save = FALSE) {
  mrggsave(..., draw = FALSE, .save = FALSE)
}
