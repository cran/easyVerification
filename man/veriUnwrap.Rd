% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/veriUnwrap.R
\name{veriUnwrap}
\alias{veriUnwrap}
\title{Unwrap Arguments and Hand Over to Verification Function}
\usage{
veriUnwrap(
  x,
  verifun,
  nind = c(nens = ncol(x) - 1, nref = 0, nobs = 1, nprob = 0, nthresh = 0),
  ref.ind = NULL,
  ...
)
}
\arguments{
\item{x}{n x k + 1 matrix with n forecasts of k ensemble members plus the
verifying observations}

\item{verifun}{character string with function name to be executed}

\item{nind}{named vector with number of ensemble members, ensemble members of
reference forecasts, observations (defaults to 1), probability or absolute
thresholds (see details)}

\item{ref.ind}{list with specifications for the reference forecast
(see details)}

\item{...}{additional arguments passed on to \code{verifun}}
}
\description{
Decomposes input arguments into forecast, verifying observations, and
reference forecast and hands these over to the function provided.
}
\details{
Forecast verification metrics are only computed for forecasts with
  non-missing verifying observation and at least one non-missing ensemble
  member. Metrics for all other forecasts are set to missing. For aggregate
  metrics (e.g. skill scores) the metric is computed over non-missing
  observation/forecast pairs only.

  For computation of skill scores, reference forecasts can be provided. That
  is, the first \code{nens} columns of \code{x} contain the forecasts, the
  \code{(nens + 1):(ncol(x) - 1)} following columns contain the reference
  forecast, and the final column contains the observations. If no reference
  forecast is provided (i.e. \code{ncol(x) == nens + 1}), a climatological
  forecast is constructed from the \code{n} verifying observations.

  The elements of vector \code{nind} have to be named with \code{nens}
  containing the number of ensemble members, \code{nref} the number of
  ensemble members in the reference forecast for skill scores, \code{nobs}
  the number of observations (only one supported), \code{nprob} the number of
  probability thresholds, and \code{nthresh} the number of absolute threshold
  for conversion of continuous forecasts to category forecasts.

  \code{ref.ind} specifies the set-up of the climatological reference
  forecast for skill scores if no explicit reference forecast is provided
  (see \code{\link{indRef}}). Also, \code{ref.ind} is used to determine the
  baseline to estimate the percentile-based category boundaries to convert
  continuous forecasts to category probabilities.
}
\note{
Out-of-sample reference forecasts are now fully supported.
}
\seealso{
\code{\link{veriApply}}
}
