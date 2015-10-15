# convert2prob.R convert to probability / categorical forecast
#
#     Copyright (C) 2015 MeteoSwiss
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

#' Convert to probability / categorical forecast
#' 
#' @param x input vector or matrix
#' @param prob thresholds for categorical forecasts (defaults to NULL)
#' @param threshold absolute thresholds for categorical forecasts (defaults to NULL)
#' 
#' @details
#' In case both \code{prob} and \code{threshold} are set to \code{NULL}, the 
#' function returns the input \code{x} without modification. If \code{prob} is
#' set, a matrix with the number of occurences per class for a given quantile
#' of the full distribution (e.g. temperature above/below the median). If 
#' \code{threshold} is set, the classes are defined based on the absolute value 
#' (e.g. temperature above/below 13 deg. C). Multiple classes are supported. 
#' 
#' @return
#' Matrix of occurences per class (i.e. the number of ensemble members per class,
#' or an indicator for the observations)
#' 
#' @examples
#' tm <- toymodel()
#' 
#' ## convert to tercile forecasts (only display first forecast and obs)
#' convert2prob(tm$fcst, prob=1:2/3)[1,]
#' convert2prob(tm$obs, prob=1:2/3)[1,]
#' 
#' ## convert to category forecasts (smaller and larger than 1)
#' convert2prob(tm$fcst, threshold=1)[1,]
#' convert2prob(tm$obs, threshold=1)[1,]
#' 
#' @seealso \code{\link{veriApply}}
#' 
#' @keywords utilities
#' @export
convert2prob <- function(x, prob=NULL, threshold=NULL){
  stopifnot(is.vector(x) | is.matrix(x))
  stopifnot(any(!is.na(x)))
  if (!is.null(prob) & !is.null(threshold)){
    stop('Both probability and absolute thresholds provided')
  } 
  ## convert probability to absolute threshold
  if (is.numeric(prob)){
    threshold <- quantile(x, prob, na.rm=T, type=8)
  }
  ## compute occurence per class
  if (is.numeric(threshold)){
    threshold <- sort(threshold)
    nclass <- length(threshold) + 1
    #xtmp <- array(findInterval(x, threshold) + 1, dim(as.matrix(x)))
    xtmp <- array(apply(sapply(threshold, function(y) c(x) > y), 1, sum), dim(as.matrix(x))) + 1
    xout <- t(apply(xtmp, 1, tabulate, nbins=nclass))
    xout[apply(as.matrix(is.na(x)), 1, any),] <- NA
  } else {
    xout <- x
  }
  return(xout)
}