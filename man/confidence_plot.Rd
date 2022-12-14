% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{confidence_plot}
\alias{confidence_plot}
\title{Plot to check reported change-points}
\usage{
confidence_plot(
  model,
  scale = "percentage",
  index_values = NULL,
  index_variable_name = "Index",
  pkg = "base"
)
}
\arguments{
\item{model}{A \code{blockcpd} model object.}

\item{scale}{A string describing the scale which the y-scale should is
plotted. Possible values are "percentage", "probability" and "frequency".}

\item{index_values}{A numerical vector of size ncol that contains the values
of the the variable corresponding to the change points.}

\item{index_variable_name}{Name of the variable segmented.}

\item{pkg}{Graphical package to be used for plotting. Current values are
"base".}
}
\value{
No return value.
}
\description{
Plots   the estimates of how likely it is for the model to detect a change at
any given point. True change-points should have confidence near $100\%$,
while non change-points should have a confidence near $0\%$. It might also be
difficult to detect a true change-point at the given sample size. In this
case, it should fluctuate in the middle.
}
\examples{
td = rcpd(nrow = 10, ncol = 10)
model = fit_blockcpd(td$data_matrix, bootstrap = TRUE)
confidence_plot(model)
}
