
is_glist <- function(x) "gList" %in% class(x)

##' Save plot objects as .pdf file after labeling with Source graphic and
##' Source code labels
##'
##' @param x an object or list of objects of class \code{gg}
##' @param script the name of the script generating the \code{gg} objects
##' @param stem to form the name of the output \code{.pdf} file
##' @param dir output directory for \code{.pdf} file
##' @param prefix gets prepended to the output file path in the Source graphic
##' label
##' @param onefile passed to \code{\link{pdf}}
##' @param fontsize for Source graphic and Source code labels
##' @param textGrob.x passed to \code{textGrob} (as \code{x})
##' @param textGrob.y passed to \code{textGrob} (as \code{y})
##' @param margin numeric vector of length 4 or 1 to set top, right, bottom,
##' left margin
##' @param unit unit to go along with margin sizes
##' @param nosave logical; if \code{TRUE}, return the labeled objects
##' @param arrange logical; if \code{TRUE}, arrange the ggplot objects on a
##' single page with \code{arrangeGrob}
##' @param labsep character separator (or newline) for Source code and Source
##' graphic labels
##' @param draw if \code{TRUE}, the image is printed but not saved
##' @param ... other arguments passed to \code{\link{pdf}} or \code{arrangeGrob}
##'
##' @details
##' \itemize{
##'   \item This function will \code{\link{require}} the \code{gridExtra}
##'   package
##' }
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
##' dir <- tempdir()
##'
##'
##' p1 <- ggplot(data=Theoph) + geom_line(aes(x=Time, y=conc, group=Subject))
##' p2 <- ggplot(data=Theoph) + geom_line(aes(x=Time, y=conc)) + facet_wrap(~Subject)
##'
##' mrggsave(p1, Script, "plot1", dir=dir)
##' mrggsave(p2, Script, "plot2", dir=dir)
##'
##' mrggsave(list(p1,p2), Script, "both_plots", dir=dir)
##' mrggsave(list(p1,p2), Script, "separate_files", onefile=FALSE, dir=dir)
##'
##' mrggsave(p1, Script, "different_shape", width=10, height=4, dir=dir)
##'
##' mrggsave(list(p1,p2), Script, "onepage", dir=dir, arrange=TRUE, nrow=1, ncol=2)
##'
##' require(GGally)
##'
##' p3 <- ggpairs(data)
##' mrggsave(p3, Script, "ggally_plot", dir = dir)
##'
##' @export
mrggsave <- function(x,...) UseMethod("mrggsave")

##' @rdname mrggsave
##' @export
mrggsave.trellis <- function(x,
                             script,
                             stem="Rplot",
                             dir="../deliv/figure",
                             prefix=gsub("^\\.\\./","./",dir),
                             onefile=TRUE,arrange=FALSE,labsep = "\n",
                             fontsize = 7,
                             textGrob.x = 0.0025, textGrob.y = 1,
                             margin = c(0.1, 0.1, 0.7, 0.1),
                             unit = "cm",
                             draw = FALSE,
                             nosave=FALSE,...) {

  if(length(margin)!=4) {
    if(length(margin)!=1) {
      stop("margin must be length 4 or length 1")
    }
    margin <- rep(margin,4)
  }

  if(!inherits(x,"list")) x <- list(x)

  if(arrange) {
    onefile <- TRUE
    x <- lapply(x, arrangeGrob)
    x <- gList(arrangeGrob(grobs=x,...))
  }

  n  <- length(x)

  if(!onefile) {
    pdffile <- paste(file.path(dir,stem), "%03d.pdf", sep="")
    file <- paste(file.path(prefix,stem), sprintf("%03d", 1:n), ".pdf",sep="")
    outfile <- sprintf(pdffile, seq_len(n))
  } else {
    pdffile <- paste(file.path(dir,stem), ".pdf", sep="")
    file <- paste(file.path(prefix,stem), ".pdf", sep="")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", seq_len(n))
  }

  label <- paste("\n\n\n","Source code: ", script,
                 labsep,"Source graphic: ", file, sep="")

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
    lapply(x,grid.draw)
    return(invisible(x))
  }

  if(nosave) return(invisible(x))

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) gridExtra::grid.arrange(x[[i]])
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
                            stem="Rplot",
                            dir="../deliv/figure",
                            prefix=gsub("^\\.\\./","./",dir),
                            onefile=TRUE,arrange=FALSE,labsep = "\n",
                            fontsize = 7,
                            textGrob.x = 0.01, textGrob.y = 0.6,
                            margin = c(0.1, 0.2, 0.7, 0.1),
                            unit = "cm",
                            draw = FALSE,
                            nosave=FALSE,...) {

  if(length(margin)!=4) {
    if(length(margin)!=1) {
      stop("margin must be length 4 or length 1")
    }
    margin <- rep(margin,4)
  }

  if(!inherits(x,"list")) x <- list(x)

  x <- lapply(x, function(xx) {
    xx + theme(
      plot.margin = ggplot2::margin(
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
    pdffile <- paste(file.path(dir,stem), "%03d.pdf", sep="")
    file <- paste(file.path(prefix,stem), sprintf("%03d", 1:n), ".pdf",sep="")
    outfile <- sprintf(pdffile, 1:n)
  } else {
    pdffile <- paste(file.path(dir,stem), ".pdf", sep="")
    file <- paste(file.path(prefix,stem), ".pdf", sep="")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", 1:n)
  }

  label <- paste("Source code: ", script, labsep,"Source graphic: ", file,sep="")

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
      lapply(x,grid.draw)
    }
    return(invisible(x))
  }

  if(nosave) return(invisible(x))

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) grid.arrange(x[[i]])
  grDevices::dev.off()

  return(invisible(outfile))
}


##' @rdname mrggsave
##' @export
mrggsave.ggmatrix <- function(x,
                              script,
                              stem="Rplot",
                              dir="../deliv/figure",
                              prefix=gsub("^\\.\\./","./",dir),
                              onefile=TRUE,arrange=FALSE,labsep = "\n",
                              fontsize = 7,
                              textGrob.x = 0.01, textGrob.y = 0.6,
                              margin = c(0.1, 0.2, 0.7, 0.1),
                              unit = "cm",
                              draw = FALSE,
                              nosave=FALSE,...) {

  if(arrange) {
    warning("cannon't arrange ggmatrix objects")
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
  x <- GGally::ggmatrix_gtable(x)
  x <- gtable::gtable_add_padding(x,unit(margin, unit))

  if(!inherits(x,"list")) x <- list(x)

  n  <- length(x)

  if(!onefile) {
    pdffile <- paste(file.path(dir,stem), "%03d.pdf", sep="")
    file <- paste(file.path(prefix,stem), sprintf("%03d", 1:n), ".pdf",sep="")
    outfile <- sprintf(pdffile, seq_len(n))
  } else {
    pdffile <- paste(file.path(dir,stem), ".pdf", sep="")
    file <- paste(file.path(prefix,stem), ".pdf", sep="")
    outfile <- pdffile
    if(n>1) file <-  paste(file, "page:", seq_len(n))
  }

  label <- paste("Source code: ", script, labsep,"Source graphic: ", file,sep="")

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
      lapply(x,grid.draw)
    }
    return(invisible(x))
  }


  if(nosave) return(invisible(x))

  args <- list(...)
  args$file <- pdffile
  args$onefile <- onefile
  args <- args[names(args) %in% names(formals(grDevices::pdf))]

  do.call(grDevices::pdf, args)
  for(i in seq_along(x)) grid.arrange(x[[i]])
  dev.off()

  return(invisible(outfile))
}


##' @export
##' @rdname mrggsave
mrggdraw <- function(..., save = FALSE) {
  mrggsave(..., draw = TRUE, save = save)
}

##' @rdname mrggsave
##' @export
mrggsave.list <- function(x, ..., arrange = FALSE) {

  cl <- lapply(x, class)
  cl <- unlist(lapply(cl, paste, collapse = "-"), use.names=FALSE)
  if(any(cl == "gg-ggmatrix")) {
    stop("gg-ggmatrix objects are not allowed in list", call. = FALSE)
  }
  if(arrange) {
    if(!all(cl==cl[1])) {
      stop("not all objects are of the same class", call. = FALSE)
    }
  } else {
    if(any(cl=="trellis")) {
      cl <- "trellis"
    }
  }

  cl <- cl[1]

  if(identical(cl, "gg-ggplot")) {
    return(mrggsave.ggplot(x,arrange = arrange,...))
  }

  if(identical(cl, "trellis")) {
    return(mrggsave.trellis(x,arrange = arrange,...))
  }

  stop("invalid object of class: ", cl, call. = FALSE)
}




