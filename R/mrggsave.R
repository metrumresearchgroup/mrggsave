
#' Label, arrange, and save graphics
#'
#' Save plot objects as .pdf file after labeling with Source graphic and
#' Source code labels.
#'
#'
#'
#' @param x an object or list of objects of class \code{gg}
#' @param script the name of the script generating the \code{gg} objects
#' @param stem to form the name of the output \code{.pdf} file
#' @param tag if specified, stem is overwritten by pasting \code{script}
#' and \code{tag} together
#' @param dir output directory for graphic file
#' @param md5dir output directory below \code{dir} for md5 hash of the plot(s)
#' being saved; the md5 has is computed and saved when a character value for the
#' directory name is passed
#' @param prefix gets prepended to the output file path in the Source
#' graphic, label
#' @param onefile passed to \code{\link{pdf}}
#' @param fontsize for Source graphic and Source code labels
#' @param textGrob.x passed to \code{textGrob} (as \code{x})
#' @param textGrob.y passed to \code{textGrob} (as \code{y})
#' @param just passed to \code{textGrob} (as \code{just})
#' @param ypad integer number of newlines to separate annotation
#' from x-axis title
#' @param width passed to \code{\link{pdf}}; should be less than 5 in.
#' for portrait figure
#' @param height passed to \code{\link{pdf}}; should be less than 7 in.
#' for portrait figure
#' @param dev the device to use
#' @param res passed to \code{\link{png}}
#' @param units passed to \code{\link{png}}
#' @param position force the graphic annotation to locate to the left or right
#' @param labeller a function that creates the plot annotation; the function
#' should receive a single argument (\code{x}) which is an environment
#' containing various items that might go into the label; pass `NULL` to
#' omit the label on the plot
#' @param .save logical; if \code{FALSE}, return the labeled objects
#' @param arrange logical; if \code{TRUE}, arrange the ggplot objects on a
#' single page with \code{arrangeGrob}
#' @param ncol passed to \code{\link{arrangeGrob}}
#' @param labsep character separator (or newline) for Source code and
#' Source graphic labels
#' @param pre_label text to include before annotation; separate lines prior
#' to Source code label; see details
#' @param post_label text to include after annotation; separate lines after
#' Source graphic; see details
#' @param draw if \code{TRUE}, the plot is drawn using \code{\link{draw_newpage}}
#' @param use_names if \code{TRUE}, the names from a list of plots will be used
#' as the stems for output file names
#' @param envir environment to be used for string interpolation in
#' stem and tag
#' @param ... other arguments passed to \code{mrggsave_common} and then
#' on to \code{\link{pdf}} and \code{arrangeGrob}
#'
#' @details
#' Methods are provided for \code{ggplot} output, \code{lattice}
#' output, and \code{ggmatrix} objects (produced by
#' \code{GGally::ggpairs}).  Either a single plot object
#' or a list of objects can be passed in.  If a list of objects
#' are passed in, the plots may be written to a single file (default)
#' or multiple files (if \code{onefile} is \code{FALSE}).
#' Alternatively, \code{ggplots} and \code{lattice plots}
#' can be arranged on a single page when
#' \code{arrange} is \code{TRUE}.  \code{ggmatrix} objects
#' cannot be arranged.  An error is generated if different
#' object types are passed in a single list.
#'
#' By default, the output file name is generated from
#' the script name and the value in \code{tag}.  For example,
#' when the script is named \code{vpc_figures} and the tag
#' is passed as \code{_by_dose_group}, the output file name
#' will be \code{vpc_figures_by_dose_group.pdf}.  Alternatively,
#' the user can specify the complete stem of the file
#' name with the \code{stem} argument.
#'
#' When \code{.save} is \code{FALSE}, \code{mrggsave}
#' always returns a list of table grobs.  If a single
#' plot was passed, the return value in this case
#' is a list of length 1.
#'
#' \code{mrgglabel} calls \code{mrggsave} and
#' neither draws nor saves the plot, but
#' returns the annotated plots as table grob.
#'
#' \code{pre_label} and \code{post_label} are collapsed with newline if
#' supplied by the user, allowing multiple lines to be added before or
#' after the standard annotation.
#'
#' @seealso \code{\link{mrggdraw}}, \code{\link{mrggsave_list}}
#'
#' @examples
#' data(Theoph)
#' require(ggplot2)
#'
#' x <- runif(1000,10,100)
#' y <- 0.3*x + rnorm(length(x),0,20)
#' data <- data.frame(x = x, y = y)
#'
#' Script <- "example.R"
#'
#' # NOTE: see default value for dir argument, which should be appropriate
#' # for project work
#'
#' # Changing it here only for the example
#' options(mrggsave.dir = tempdir())
#'
#'
#' p1 <- ggplot(data=Theoph) +
#'   geom_line(aes(x=Time, y=conc, group=Subject))
#'
#' p2 <- ggplot(data=Theoph) +
#'   geom_line(aes(x=Time, y=conc)) +
#'   facet_wrap(~Subject)
#'
#' mrggsave(p1, Script, "_plot1")
#'
#' \dontrun{
#' mrggsave(p2, Script, "_plot2")
#' }
#'
#' mrggsave(list(p1,p2), Script, "both_plots")
#'
#' \dontrun{
#' mrggsave(list(p1,p2), Script, "separate_files", onefile=FALSE)
#' }
#'
#' mrggsave(p1, Script, "different_shape", width=10, height=4)
#'
#' \dontrun{
#' mrggsave(list(p1,p2), Script, "onepage", arrange=TRUE, ncol=2)
#'
#' stopifnot(require(GGally))
#'
#' p3 <- ggpairs(data)
#'
#' mrggsave(p3, Script, "ggally_plot")
#' }
#' @export
mrggsave <- function(x, ...) {
  UseMethod("mrggsave")
}

#' @rdname mrggsave
#' @export
mrggsave.ggplot <- function(x, ..., ypad = 2,
                            arrange = FALSE,
                            ncol = 1,
                            onefile = TRUE) {

  if(ncol > 1) arrange <- TRUE

  if(!inherits(x,"list")) {
    x <- list(x)
  }

  if(arrange) {
    onefile <- TRUE
    x <- gList(arrangeGrob(grobs=x, ncol = ncol, ...))
  }

  mrggsave_common(
    x = x, ypad = ypad,onefile = onefile, arrange = arrange, ...
  )
}

#' @rdname mrggsave
#' @export
mrggsave.ggmatrix <- function(x, ..., ypad = 4, arrange = FALSE,
                              onefile = TRUE) {

  if(!inherits(x,"list")) {
    x <- list(x)
  }

  x <- lapply(x, mrggsave_prep_object.ggmatrix)

  if(arrange) {
    onefile <- TRUE
    x <- gList(arrangeGrob(grobs=x,...))
  }

  mrggsave_common(
    x = x, ypad = ypad, arrange = arrange, ...
  )
}

#' @rdname mrggsave
#' @export
mrggsave.gList <- function(...) {
  mrggsave_common(...)
}

#' @rdname mrggsave
#' @export
mrggsave.gtable <- function(...) {
  mrggsave.ggplot(...)
}

#' @rdname mrggsave
#' @export
mrggsave.trellis <- function(x, ..., ypad = 3, arrange = FALSE, ncol = 1,
                             onefile = TRUE) {

  if(ncol > 1) arrange <- TRUE

  if(!inherits(x,"list")) {
    x <- list(x)
  }

  if(arrange) {
    onefile <- TRUE
    x <- lapply(x, arrangeGrob)
    x <- gList(arrangeGrob(grobs=x,ncol = ncol, ...))
  }

  mrggsave_common(
    x = x, ypad = ypad, arrange = arrange, onefile = onefile, ...
  )
}

#' @rdname mrggsave
#' @export
mrggsave.patchwork <- function(x,...) {
  assert_that(requireNamespace("patchwork"))
  mrggsave(patchwork::patchworkGrob(x),...)
}

#' @export
mrggsave.ggsurvplot <- function(x,...) {
  mrggsave_common(mrggsave_prep_object(x), ...)
}

#' @rdname mrggsave
#' @export
mrggsave.list <- function(x, ..., arrange = FALSE, use_names = FALSE) {

  if(inherits(x, "named-plots")) use_names <-  TRUE
  if(missing(use_names) && is_named(x)) use_names <- TRUE
  # dig <- sapply(x, digest)

  if(use_names) {
    stem <- names(x)
    if(is.null(stem)) {
      stop("the plot list must be named when `use_names` is TRUE.", call.=FALSE)
    }
    if(!all(nchar(stem) > 0)) {
      stop("all plot names must at least one character.", call.=FALSE)
    }
    args <- list(...)
    args$arrange <- arrange
    tag <- args$tag
    args$tag <- NULL
    context <- NULL
    if(inherits(x, "needs-context")) {
      context <- args$script
      if(is.null(context)) {
        mrg.script <- getOption("mrg.script", context)
        context <- getOption("mrggsave.use.context", mrg.script)
      }
      context <- no_r_ext(context)
    }
    ans <- lapply(seq_along(x), function(i) {
      args$stem <- paste0(c(context, stem[i], tag), collapse = "-")
      args$x <- x[[i]]
      do.call(mrggsave, args)
    })

    if(getOption("mrggsave.return.more", FALSE)) {
      names(ans) <- stem
      return(invisible(ans))
    }

    return(invisible(unlist(ans, use.names = FALSE)))
  }

  x <- flatten_plots(x)

  cl <- scan_list_cl(x)

  if(all(cl$cl == "gg-ggplot")) {
    return(mrggsave.ggplot(x, arrange = arrange, ...))
  }

  if(all(cl$cl == "trellis")) {
    return(mrggsave.trellis(x, arrange = arrange, ...))
  }

  if(all(cl$ggmatrix)) {
    return(mrggsave.ggmatrix(x, arrange = arrange, ...))
  }

  x <- lapply(x, mrggsave_prep_object)

  mrggsave.ggplot(x, arrange = arrange, ...)
}

#' @rdname mrggsave
#' @export
mrggsave.gg <- function(x, ...) {
  NextMethod()
}

#' @rdname mrggsave
#' @export
mrggsave.gTree <- function(x, ...) {
  mrggsave_common(mrggsave_prep_object(x), ...)
}

#' @export
#' @rdname mrggsave
mrgglabel <- function(..., draw = FALSE, .save = FALSE) {
  mrggsave(..., draw = FALSE, .save = FALSE)
}

eps <- function(...) {
  postscript(..., paper = "special", onefile = FALSE, horizontal = FALSE)
}

#' @rdname mrggsave
#' @export
mrggsave_common <- function(x,
                            script = getOption("mrg.script", NULL),
                            tag = NULL,
                            width = 5, height = 5,
                            stem = "Rplot",
                            dir = getOption("mrggsave.dir","../deliv/figure"),
                            md5dir = getOption("mrggsave.md5dir", NULL),
                            prefix = NULL,#gsub("^\\.\\./","./",dir),
                            onefile = TRUE,
                            arrange = FALSE,
                            draw = FALSE,
                            .save = TRUE,
                            ypad = 3,
                            labsep = "\n",
                            pre_label = NULL,
                            post_label = NULL,
                            fontsize = 7,
                            textGrob.x = 0.01,
                            textGrob.y = unit(0.1,"in"),
                            just = c('left','bottom'),
                            dev = getOption("mrggsave.dev", "pdf"),
                            res = 150,
                            units = "in",
                            position = getOption("mrggsave.position", "default"),
                            labeller = getOption("mrggsave.label.fun", label.fun),
                            envir = sys.frame(-1),
                            ...) {

  n  <- length(x)

  stopifnot(is.character(dev))
  dev <- cvec_cs(dev)
  more_dev <- NULL
  if(length(dev) > 1) {
    more_dev <- dev[-1]
    dev <- dev[1]
  }

  if(dev=="pdf") {
    onefile <- isTRUE(onefile) | n==1
  } else {
    onefile <- length(x)==1
  }

  ext <- gsub("cairo_pdf", "pdf", dev, fixed = TRUE)
  ext <- paste0(".", ext)

  if(is.null(script)) {
    stop(
      c("please specify the script name either as an argument (`script`) ",
        "or an option (`mrg.script`)"),
      call. = FALSE
    )
  }

  if(!is.null(tag)) {
    context <- getOption("mrggsave.use.context", script)
    stem <- make_stem(context, tag)
  } else {
    stem <- paste0(stem, collapse = "-")
  }

  stem <- glue(stem, .envir = envir)

  if(isTRUE(getOption("mrggsave.file.tolower", FALSE))) {
    stem <- tolower(stem)
  }

  file <- list(
    device = NULL,  # gets passed to the device call
    out   =  NULL,  # the actual output file name
    label =  NULL   # gets printed in the annotation label
  )
  if(!onefile) {
    file$device <- file.path(dir, paste0(stem,"%03d", ext)) #paste0(file.path(dir,stem),"%03d", ext)
    file$out <- sprintf(file$device, seq(n))
    file$label <- basename(file$out) #paste0(stem, sprintf("%03d", seq(n)), ext)
    if(is.character(prefix)) file$label <- file.path(prefix, file$label)
  } else {
    file$label <- paste0(stem,ext)
    file$device <- file.path(dir, file$label) #paste0(file.path(dir,stem), ext)
    file$out <- file$device
    file$label <- paste0(stem, ext)
    if(is.character(prefix)) file <- file.path(prefix, file$label)
    if(n > 1) file$label <-  paste(file$label, "page:", seq(n))
  }

  if(is.character(md5dir)) {
    dump_md5(x, dir = dir, md5dir = md5dir, file, onefile = onefile)
  }

  pad <- paste0(rep("\n", as.integer(ypad)), collapse = "")

  d <- list2env(
    list(
      pad = pad,
      pre_label = pre_label,
      post_label = post_label,
      source_code = script,
      labsep = labsep,
      source_graphic = file$label,
      n = n,
      file = file$device,
      position = position,
      textGrob.x = textGrob.x,
      textGrob.y = textGrob.y,
      fontsize = fontsize,
      just = just
    ),
    parent = .GlobalEnv
  )

  position <- match.arg(d$position, c("default", "left", "right"))
  if(position != "default") {
    if(position=="left") {
      d$just <- c("left", "bottom")
      d$textGrob.x <- unit(0.015, "npc")
    }
    if(position=="right") {
      d$just <- c("right", "bottom")
      d$textGrob.x <- unit(0.985, "npc")
    }
  }

  x <- annotate_graphic(x, d, labeller)

  if(draw) {
    if(is_glist(x)) {
      draw_newpage(x)
    } else {
      .foo <- lapply(x,draw_newpage)
    }
  }

  if(!.save) {
    return(invisible(x))
  }

  args <- list(
    onefile = onefile, width = width, height = height, res = res,
    units = units, file = file$device, filename = file$device
  )

  if(dev=="eps") {
    dev <- "postscript"
    args$paper <- "special"
    args$onefile <- FALSE
    args$horizontal <- FALSE
  }

  if(dev=="ps") {
    dev <- "postscript"
  }

  args <- c(args, list(...))
  args <- args[names(args) %in% names(formals(dev))]

  do.call(dev, args)
  for(i in seq_along(x)) {
    grid.arrange(x[[i]])
  }
  grDevices::dev.off()

  if(!is.null(more_dev)) {
    mrggcall <- match.call()
    for(d in more_dev) {
      mrggcall$dev <- d
      foo <- eval(mrggcall, sys.frame(-1))
    }
  }

  if(getOption("mrggsave.return.more", FALSE)) {
    x <- list(
      outfile = file$out,
      label = d$label,
      source_graphic = d$source_graphic,
      source_code = d$source_code
    )
    return(invisible(x))
  }

  return(invisible(file$out))
}

annotate_graphic <- function(x, d, labeller) {
  if(!is.function(labeller)) {
    d$label <-  rep("", d$n)
    return(lapply(x, arrangeGrob))
  }
  d$label <- labeller(d)
  if(length(d$label) != d$n) {
    nn <- length(d$label)
    stop("`label` must be length ", d$n," (not ", nn, ")", call. = FALSE)
  }
  for(i in seq_along(x)) {
    x[[i]] <- arrangeGrob(
      x[[i]],
      bottom = textGrob(
        gp = gpar(fontsize = d$fontsize),
        just = d$just,
        y = d$textGrob.y,
        x = d$textGrob.x,
        label = d$label[[i]]
      )
    )
  }
  x
}

dump_md5 <- function(x, dir, md5dir, file, onefile) {
  n <- length(x)
  path <- file.path(dir, md5dir)
  onefile <- isTRUE(onefile)
  if(!dir.exists(path)) dir.create(path)
  dig <- sapply(x, digest)
  pagen <- seq(n)
  if(!onefile) pagen <- rep(1,n)
  dig <- paste0("page ", formatC(pagen, width = 3, flag = "0"), ": ", dig)
  md5file <- paste0(basename(file$out), ".hash")
  if(onefile) {
    writeLines(con = file.path(path, md5file), text = dig)
  } else {
    for(i in seq_along(md5file)) {
      writeLines(con = file.path(path, md5file[[i]]), text = dig[[i]])
    }
  }
  return(invisible(NULL))
}

scan_list_cl <- function(x) {
  cl <- lapply(x, class)
  cl <- unlist(lapply(cl, paste, collapse = "-"), use.names=FALSE)
  list(ggmatrix = cl == "gg-ggmatrix",
       #ggassemble = cl=="ggassemble-gg-ggplot",
       cl = cl)
}

#' Save the last plot using mrggsave
#' @param stem passed to [mrggsave]
#' @param script passed to [mrggsave]
#' @param ... passed to [mrggsave]
#' @export
mrggsave_last <- function(stem, script = getOption("mrg.script", NULL), ...) {
  mrggsave(last_plot(), stem = stem, script = script, ...)
}
