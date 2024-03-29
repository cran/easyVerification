# veriApply.R Apply Verification Metrics to Large Datasets
#
#     Copyright (C) 2016 MeteoSwiss
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' Apply Verification Metrics to Large Datasets
#'
#' This wrapper applies verification metrics to arrays of forecast ensembles and
#' verifying observations. Various array-based data formats are supported.
#' Additionally, continuous forecasts (and observations) are transformed to
#' category forecasts using user-defined absolute thresholds or percentiles of
#' the long-term climatology (see details).
#'
#' @param verifun Name of function to compute verification metric (score, skill
#'   score)
#' @param fcst array of forecast values (at least 2-dimensional)
#' @param obs array or vector of verifying observations
#' @param fcst.ref array of forecast values for the reference forecast (skill
#'   scores only)
#' @param tdim index of dimension with the different forecasts
#' @param ensdim index of dimension with the different ensemble members
#' @param prob probability threshold for category forecasts (see below)
#' @param threshold absolute threshold for category forecasts (see below)
#' @param strategy type of out-of-sample reference forecasts or  namelist with
#'   arguments as in \code{\link{indRef}} or list of indices for each
#'   forecast instance
#' @param na.rm logical, should incomplete forecasts be used?
#' @param fracmin fraction of forecasts that are not-missing for forecast to
#'   be evaluated. Used to determine \code{nmin} when \code{is.null(nmin)}
#' @param nmin number of forecasts that are not-missing for forecast to
#'   be evaluated. If both \code{nmin} an d \code{fracmin} are set, \code{nmin}
#'   takes precedence
#' @param parallel logical, should parallel execution of verification be used
#'   (see below)?
#' @param maxncpus upper bound for self-selected number of CPUs
#' @param ncpus number of CPUs used in parallel computation, self-selected
#'   number of CPUs is used when \code{is.null(ncpus)} (the default).
#' @param ... additional arguments passed to \code{verifun}
#'
#' @section List of functions to be called: The selection of verification
#'   functions supplied with this package and as part of
#'   \code{SpecsVerification} can be enquired using
#'   \code{ls(pos='package:easyVerification')} and
#'   \code{ls(pos='package:SpecsVerification')} respectively. Please note,
#'   however, that only some of the functions provided as part of
#'   \code{SpecsVerification} can be used with \code{\link{veriApply}}.
#'   Functions that can be used include for example the (fair) ranked
#'   probability score \code{\link[SpecsVerification]{EnsRps}},
#'   \code{\link[SpecsVerification]{FairRps}}, and its skill score
#'   \code{\link[SpecsVerification]{EnsRpss}},
#'   \code{\link[SpecsVerification]{FairRpss}}, or the continuous ranked
#'   probability score \code{\link[SpecsVerification]{EnsCrps}}, etc.
#'
#' @section Conversion to category forecasts: To automatically convert
#'   continuous forecasts into category forecasts, absolute (\code{threshold})
#'   or relative thresholds (\code{prob}) have to be supplied. For some scores
#'   and skill scores (e.g. the ROC area and skill score), a list of categories
#'   will be supplied with categories ordered. That is, if \code{prob = 1:2/3}
#'   for tercile forecasts, \code{cat1} corresponds to the lower tercile,
#'   \code{cat2} to the middle, and \code{cat3} to the upper tercile.
#'
#'   Absolute and relative thresholds can be supplied in various formats. If a
#'   vector of thresholds is supplied with the \code{threshold} argument, the
#'   same threshold is applied to all forecasts (e.g. lead times, spatial
#'   locations). If a vector of relative thresholds is supplied using
#'   \code{prob}, the category boundaries to be applied are computed separately
#'   for each space-time location. Relative boundaries specified using
#'   \code{prob} are computed separately for the observations and forecasts, but
#'   jointly for all available ensemble members.
#'
#'   Location specific thresholds can also be supplied. If the thresholds are
#'   supplied as a matrix, the number of rows has to correspond to the number of
#'   forecast space-time locations (i.e. same length as
#'   \code{length(fcst)/prod(dim(fcst)[c(tdim, ensdim)])}). Alternatively, but
#'   equivalently, the thresholds can also be supplied with the dimensionality
#'   corresponding to the \code{obs} array with the difference that the forecast
#'   dimension in \code{obs} contains the category boundaries (absolute or
#'   relative) and thus may differ in length.
#'
#' @section Out-of-sample reference forecasts:\code{strategy} specifies the
#'   set-up of the climatological reference forecast for skill scores if no
#'   explicit reference forecast is provided. The default is \code{strategy = "none"},
#'   that is all available observations are used as equiprobable
#'   members of a reference forecast. Alternatively, \code{strategy = "crossval"}
#'   can be used for leave-one-out crossvalidated reference forecasts,
#'   or \code{strategy = "forward"} for a forward protocol (see \code{\link{indRef}}).
#'
#'   Alternatively, a list with named parameters corresponding to the input
#'   arguments of \code{\link{indRef}} can be supplied for more fine-grained
#'   control over standard cases. Finally, also a list with observation indices
#'   to be used for each forecast can be supplied (see \code{\link{generateRef}}).
#'
#' @section Parallel processing: Parallel processing is enabled using the
#'   \code{\link[parallel]{parallel}} package. Parallel verification is using
#'   \code{ncpus} \code{FORK} clusters or, if \code{ncpus} are not specified,
#'   one less than the autod-etected number of cores. The maximum number of cores
#'   used for parallel processing with auto-detection of the number of available
#'   cores can be set with the \code{maxncpus} argument.
#'
#'   Progress bars are available for non-parallel computation of the
#'   verification metrics. Please note, however, that the progress bar only
#'   indicates the time of computation needed for the actual verification
#'   metrics, input and output re-arrangement is not included in the progress
#'   bar.
#'
#' @note If the forecasts and observations are only available as category
#'   probabilities (or ensemble counts as used in \code{SpecsVerification}) as
#'   opposed to as continuous numeric variables, \code{veriApply} cannot be used
#'   but the atomic verification functions for category forecasts have to be
#'   applied directly.
#'
#'   Out-of-sample reference forecasts are not fully supported for
#'   categorical forecasts defined on the distribution of forecast values (e.g.
#'   using the argument \code{prob}). Whereas only the years specified in
#'   \code{strategy} are used for the reference forecasts, the probability
#'   thresholds for the reference forecasts are defined on the collection of
#'   years specified in \code{strategy}.
#'
#' @seealso \code{\link{convert2prob}} for conversion of continuous into
#'   category forecasts (and observations)
#'
#' @examples
#' tm <- toyarray()
#' f.me <- veriApply("EnsMe", tm$fcst, tm$obs)
#'
#' ## find more examples and instructions in the vignette
#' \dontrun{
#' devtools::install_github("MeteoSwiss/easyVerification", build_vignettes = TRUE)
#' library("easyVerification")
#' vignette("easyVerification")
#' }
#'
#' @keywords utilities
#' @export
#'
veriApply <- function(verifun, fcst, obs, fcst.ref = NULL, tdim = length(dim(fcst)) - 1,
                      ensdim = length(dim(fcst)), prob = NULL, threshold = NULL,
                      strategy = "none", na.rm = FALSE, fracmin = 0.8, nmin = NULL,
                      parallel = FALSE, maxncpus = 16, ncpus = NULL,
                      ...) {
  ## check function that is supplied
  stopifnot(exists(verifun))
  stopifnot(is.function(get(verifun)))

  ## check dimensions of input
  stopifnot(is.vector(obs) | is.array(obs), is.array(fcst))
  nfdims <- length(dim(fcst))
  odims <- if (is.vector(obs)) length(obs) else dim(obs)
  nodims <- length(odims)
  otdim <- min(nodims, if (ensdim < tdim) tdim - 1 else tdim)
  ## check dimensions
  stopifnot(c(ensdim, tdim) <= nfdims)
  stopifnot(odims[-otdim] == dim(fcst)[-c(ensdim, tdim)])
  ## check that only prob or threshold are supplied
  stopifnot(is.null(prob) | is.null(threshold))

  ## check reference forecast
  if (!is.null(fcst.ref)) stopifnot(dim(fcst)[-ensdim] == dim(fcst.ref)[-ensdim])

  ## make sure that forecasts (years) and ensembles are last in forecast array
  if (ensdim != nfdims | tdim != nfdims - 1) {
    fcst <- aperm(fcst, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
    if (!is.null(fcst.ref)) fcst.ref <- aperm(fcst.ref, c(setdiff(1:nfdims, c(tdim, ensdim)), c(tdim, ensdim)))
  }
  if (otdim != nodims) {
    obs <- aperm(obs, c(setdiff(1:nodims, otdim), otdim))
  }

  ## dimensions of array to compute scores
  nens <- tail(dim(fcst), 1)
  nref <- if (!is.null(fcst.ref)) tail(dim(fcst.ref), 1) else 0
  ntim <- head(tail(dim(fcst), 2), 1)
  nrest <- length(obs) / ntim
  if (is.null(nmin)) nmin <- ceiling(fracmin * ntim)

  ## deparse strategy
  if (length(strategy) == 1 & is.character(strategy)) {
    ref.ind <- indRef(nfcst = ntim, type = strategy)
  } else if (is.list(strategy)) {
    if (all(sapply(strategy, is.numeric)) & is.null(names(strategy))) {
      ref.ind <- strategy
    } else {
      if (is.null(strategy[["nfcst"]])) strategy[["nfcst"]] <- ntim
      ref.ind <- do.call(indRef, strategy)
    }
  } else {
    stop("Format of strategy is not compatible")
  }

  ## dimensions of prob or threshold
  if (is.null(prob)) {
    nprob <- 0
  } else {
    if (is.vector(prob)) {
      prob <- t(prob)[rep(1, nrest), ]
    } else if (length(dim(prob)) == nodims) {
      prob <- aperm(prob, c(setdiff(1:nodims, otdim), otdim))
    }
    stopifnot(length(prob) %% nrest == 0)
    prob <- array(prob, c(nrest, length(prob) / nrest))
    nprob <- ncol(prob)
  }
  if (is.null(threshold)) {
    nthresh <- 0
  } else {
    if (is.vector(threshold)) {
      threshold <- t(threshold)[rep(1, nrest), ]
    } else if (length(dim(threshold)) == nodims) {
      threshold <- aperm(threshold, c(setdiff(1:nodims, otdim), otdim))
    }
    stopifnot(length(threshold) %% nrest == 0)
    threshold <- array(threshold, c(nrest, length(threshold) / nrest))
    nthresh <- ncol(threshold)
  }

  ## figure out how many 3rd dimensions are needed to write prob/thresh
  nconv <- ceiling((nprob + nthresh) / ntim)

  ## fill in xall with additional obs
  xall <- array(c(fcst, fcst.ref, obs, rep(obs * NA, nconv)), c(nrest, ntim, nens + nref + 1 + nconv))
  if (nconv > 0) {
    probthresh <- rbind(prob, threshold)
    for (j in 1:nconv) {
      ind <- seq((j - 1) * ntim + 1, min(ntim * j, ncol(probthresh))) - (j - 1) * ntim
      xall[, ind, nens + nref + 1 + j] <- probthresh[, ind]
    }
  }
  ## mask missing values
  if (na.rm) {
    ## Only verify forecast/obs pairs with at least 1 complete pair
    xmask <- apply(apply(!is.na(xall[, , 1:(nens + nref + 1), drop = F]), 1:2, all), 1, sum) >= nmin
  } else {
    ## Verify only forecast/obs pairs that are completely non-missing
    xmask <- apply(!is.na(xall[, , 1:(nens + nref + 1), drop = F]), 1, all)
  }
  ## check whether there are complete forecast/observation pairs at all
  if (!any(xmask)) {
    warning("No cases without missing values")
    xall[1, , 1:(nens + nref + 1)] <- rnorm(ncol(xall) * (nens + nref + 1))
    xmask[1] <- TRUE
    all.na <- TRUE
  } else {
    all.na <- FALSE
  }

  ## indices for re-expansion of output
  maskexpand <- rep(NA, length(xmask))
  maskexpand[xmask] <- 1:sum(xmask)

  ## run the workhorse
  Tmatrix <- function(x) if (is.matrix(x)) t(x) else as.matrix(x)

  ## check whether parallel package is available
  hasparallel <- FALSE
  ## check whether FORK nodes can be initialized
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(ncpus)) {
      ncpus <- min(max(parallel::detectCores() - 1, 1), maxncpus)
      print(paste("Number of CPUs", ncpus))
    }
    if (ncpus > 1) {
      .cl <- try(parallel::makeCluster(ncpus, type = "FORK"), silent = TRUE)
      if (!"try-error" %in% class(.cl)) hasparallel <- TRUE
    }
  }

  ## fix for FairRpss against climatological reference forecast with category
  ## boundaries defined on distribution of verifying observations
  if (verifun == "FairRpss" & !is.null(prob)) {
    ref.ind2 <- indRef(ntim)
    if (identical(ref.ind, ref.ind2)) {
      verifun <- "climFairRpss"
      message("Please note that FairRpss is computed without ensemble-size correction for the climatological reference forecast, as categories are based on quantiles of the observations and forecast probabilities are therefore known.")
    }
  }

  nind <- c(nens, nref, 1, nprob, nthresh)
  names(nind) <- c("nens", "nref", "nobs", "nprob", "nthresh")
  if (hasparallel) {
    on.exit(parallel::stopCluster(.cl))
    out <- Tmatrix(parallel::parApply(
      cl = .cl,
      X = xall[xmask, , , drop = F],
      MARGIN = 1,
      FUN = veriUnwrap,
      verifun = verifun,
      nind = nind,
      ref.ind = ref.ind,
      ...
    ))
  } else {
    out <- Tmatrix(pbapply::pbapply(xall[xmask, , , drop = F],
      MARGIN = 1,
      FUN = veriUnwrap,
      verifun = verifun,
      nind = nind,
      ref.ind = ref.ind,
      ...
    ))
  }

  ## reformat the output by converting to list
  if (is.list(out)) {
    lnames <- names(out[[1]])
    ## improve error handling with missing values in forecast
    if (is.null(lnames)) {
      errormessage <- "Only named lists can be processed. Problem with missing values in forecast?"
      outlen <- sapply(out, length)
      outnotlen <- outlen != median(outlen)
      if (any(outnotlen)) {
        fcst.i <- which(outnotlen[maskexpand])
        errormessage <- paste0(errormessage, "\n  Forecast at index ", paste(head(fcst.i), collapse = ", "), " cause(s) problems")
      }
      stop(errormessage)
    }
    olist <- list()
    for (ln in lnames) olist[[ln]] <- sapply(out, function(x) x[[ln]])
  } else {
    olist <- list(out)
  }

  ## reexpand the masked values
  olist <- lapply(olist, function(x) as.matrix(x)[maskexpand, ])
  if (all.na) {
    olist <- lapply(olist, function(x) x * NA)
  }

  ## rearrange output to original dimensions
  out <- lapply(olist, function(x) {
    if (length(x) == length(fcst)) {
      ## repermute the output
      fperm <- rep(1, nfdims)
      fperm[c(tdim, ensdim)] <- nfdims - 1:0
      if (nfdims > 2) fperm[-c(tdim, ensdim)] <- 1:(nfdims - 2)
      xout <- aperm(array(x, dim(fcst)), fperm)
    } else if (nodims > 1) {
      if (length(x) == length(obs)) {
        operm <- rep(1, nodims)
        operm[otdim] <- nodims
        if (nodims > 1) operm[-otdim] <- 1:(nodims - 1)
        xout <- if (nodims == 1) c(x) else aperm(array(x, dim(obs)), operm)
      } else if (length(x) == prod(odims[-otdim])) {
        xout <- array(x, odims[-otdim])
      }
    } else {
      xout <- x
    }
    return(xout)
  })

  ## if output list is of length one, output object within list
  if (length(out) == 1) out <- out[[1]]

  return(out)
}
