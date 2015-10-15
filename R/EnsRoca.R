# EnsRoca.R compute area under the ROC curve
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

#' Area under the ROC curve
#' 
#' Computes the area under the ROC curve given the observations
#' 
#' @param ens n x j matrix of n probability forecasts for j categories
#' @param obs n x j matrix of occurence of n verifying observations in j categories
#' 
#' @examples
#' tm <- toymodel()
#' 
#' ## compute ROC area for tercile forecasts using veriApply
#' veriApply("EnsRoca", fcst=tm$fcst, obs=tm$obs, prob=1:2/3)
#' 
#' @seealso \code{\link{veriApply}}, \code{\link{EnsRocss}}
#' 
#' @export
EnsRoca <- function(ens, obs){
  stopifnot(is.matrix(ens), is.matrix(obs), length(obs) == length(ens))
  if (any(apply(ens, 1, sum) != 1)){
    ## convert number of occurences to probabilities
    ens <- ens / apply(ens, 1, sum)
  }
  n.event <- apply(obs, 2, sum)
  n.total <- nrow(obs)
  ens.rank <- apply(ens, 2, rank)
  mean.rank <- apply(ens.rank * obs, 2, sum) / n.event
  roc.area <- (mean.rank - (n.event + 1)/2) / (n.total - n.event)
  roc.area[n.event == 0] <- NA
  roc.area <- as.list(roc.area)
  names(roc.area) <- paste0('cat', seq(along=roc.area))
  return(roc.area)
}