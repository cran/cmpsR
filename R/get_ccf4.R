#' Function to calculate the cross-correlation between two sequences
#'
#' This function is used for CMPS algorithm. 
#' @param x numeric sequence of values
#' @param y numeric sequence of values
#' @param min.overlap integer, minimal number of values in the overlap between sequences x and y to calculate a correlation value. Set to 10 percent of the maximum length of either sequence (HH: this might be problematic for CMPS)
#'
#' @return list consisting of the lag where the maximum correlation is achieved, and the maximum correlation value.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats cor complete.cases
#' @importFrom dplyr lag
#' @examples
#' data("bullets")
#' land2_3 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
#' land1_2 <- bullets$sigs[bullets$bulletland == "1-2"][[1]]
#' x <- land2_3$sig
#' y <- land1_2$sig
#' 
#' segments <- get_segs(x, len = 50)
#' 
#' ccr <- get_ccf4(y, segments$segs[[7]], 
#'                 min.overlap = length(segments$segs[[7]]))
get_ccf4 <- function (x, y, min.overlap = round(0.1 * max(length(x), length(y)))) 
{
  x <- as.vector(unlist(x))
  y <- as.vector(unlist(y))
  nx <- length(x)
  ny <- length(y)
  assert_that(nx > 0, ny > 0, nx >= ny, 
              is.numeric(x), is.numeric(y), is.numeric(min.overlap))
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  lag.max <- nx + 2 * (ny - ceiling(min.overlap)) - ny
  lags <- 0:lag.max
  lag <- lags - (ny - min.overlap)
  
  if(all(is.na(x)) | all(is.na(y)) | (ny < min.overlap)){
    cors <- rep(NA, lag.max + 1)
    return(list(lag = lag, ccf = cors))
  }
  
  x.na.count <- .Call(na_trim_c, x)
  y.na.count <- .Call(na_trim_c, y)
  
  x.narm <- x[(x.na.count[1] + 1) : (length(x) - x.na.count[2])]
  y.narm <- y[(y.na.count[1] + 1) : (length(y) - y.na.count[2])]
  nyy <- length(y.narm)
  
  xx <- c(rep(NA, nyy - min.overlap), x.narm, rep(NA, nyy - min.overlap))
  
  # make sure that y has no NA in the front or in the end
  # make sure that xx has no extra NA values; all NA are needed for shifting
  # make sure that ny >= min.overlap
  # assume x and y have been na.trim-ed
  cors <- .Call(compute_cross_corr_c, xx, y.narm, as.numeric(min.overlap))
  cors <- c(rep(NA, x.na.count[1] + y.na.count[2]), 
            cors, 
            rep(NA, x.na.count[2] + y.na.count[1]))
  return(list(lag = lag, ccf = cors))
}

#' Wrapper function for compute_cross_corr
#' @param x numeric vector, the longer sequence
#' @param y numeric vector, the shorter sequence
#' @param min.overlap numeric scalor, set the length of the minimum overlapping part
compute_cross_corr <- function(x, y, min.overlap) .Call(compute_cross_corr_c, x, y, min.overlap)

#' Wrapper function for na_trim
#' @param x numeric vector
na_trim_cmps <- function(x) .Call(na_trim_c, x)


#' Remove the leading and trailing missing values in a numeric vector
#' @param x numeric vector
#' @return a numeric vector; only the leading and trailing missing values are removed
#' @export
#' @examples
#' x <- c(NA, 1, 2, 3, 4, NA)
#' cmps_na_trim(x)
cmps_na_trim <- function(x){
  assert_that(is.numeric(x))
  
  x.na.count <- na_trim_cmps(x)
  
  x.narm <- x[(x.na.count[1] + 1) : (length(x) - x.na.count[2])]
  return(x.narm)
}

