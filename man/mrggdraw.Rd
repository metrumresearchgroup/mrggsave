% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrggdraw.R
\name{mrggdraw}
\alias{mrggdraw}
\title{Draw a plot or list of plots}
\usage{
mrggdraw(x, ncol = 1, arrange = FALSE, ...)
}
\arguments{
\item{x}{a plot or a list of plots}

\item{ncol}{passed to \code{arrangeGrob}; see details}

\item{arrange}{logical; see details}

\item{...}{passed to \code{arrangeGrob}}
}
\description{
The function will optionally arrange
}
\details{
If either ncol is greater than 1 or arrange is \code{TRUE},
then the plot(s) will be arranged via \code{arrangeGrob}.
}
