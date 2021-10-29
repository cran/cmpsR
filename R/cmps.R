#' Compute the CMPS score
#'
#' Compute the CMPS score from a list of positions of (consistent) correlation peaks.
#' @param input.ccp a list of positions for (consistent) correlation peaks
#' @param Tx integer, the tolerance zone is `+/- Tx`
#'
#' @return a list of three components:
#' * `CMPS.score`: computed CMPS score
#' * `rec.position`: the recommended position that results in the CMPS score
#' * `pos.df`: a dataframe that includes all positions and their corresponding CMPS score
#' 
#' @importFrom assertthat assert_that
#' 
#' @export
#' 
#' @examples
#' data("bullets")
#' land2_3 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
#' land1_2 <- bullets$sigs[bullets$bulletland == "1-2"][[1]]
#' x <- land2_3$sig
#' y <- land1_2$sig
#' segments <- get_segs(x, len = 50)
#' nseg <- length(segments$segs)
#' seg_scale_max <- 3
#' npeaks.set <- c(5,3,1)
#' outlength <- c(50, 100, 200)
#' 
#' ccp.list <- lapply(1:nseg, function(nseg) {
#'  ccr.list <- lapply(1:seg_scale_max, function(seg_scale) {
#'    get_ccr_peaks(y, segments, seg_outlength = outlength[seg_scale], 
#'    nseg = nseg, npeaks = npeaks.set[seg_scale])
#'  })
#' 
#'  get_ccp(ccr.list, Tx = 25)
#' })
#' cmps <- get_CMPS(ccp.list, Tx = 25)
get_CMPS <- function(input.ccp, Tx = 25) {
  
  assert_that(is.numeric(Tx))
  
  tt <- unlist(input.ccp)
  tt <- sort(tt)
  
  if(is.null(tt)) { 
    
    return(list(CMPS.score = 0, nseg = length(input.ccp), 
                congruent.pos = NA, congruent.seg = NA, 
                congruent.seg.idx = NA, pos.df = NA ))
    
    # return(list(CMPS.score = 0, rec.position = NULL, pos.df = NULL)) 
  }
  
  # pos.df <- data.frame(position = seq(min(tt), max(tt)))
  pos.df <- data.frame(position = tt)
  
  current.max <- 0
  # current.voter <- c()
  current.voter.list <- list()
  
  pos.df$cmps <- sapply(1:nrow(pos.df), function(i) {
    ccp.count <- sapply(1:length(input.ccp), function(j) {
      # if any one of the peaks belongs to the interval 
      # centered at this position, count 1
      any(abs(pos.df$position[i] - input.ccp[[j]]) <= Tx)
    })
    
    tmp.sum <- sum(ccp.count)
    
    if(tmp.sum >= current.max) {
      current.max <<- tmp.sum
      # current.voter <<- ccp.count
    }
    
    current.voter.list[[i]] <<- ccp.count
    
    return(tmp.sum)
    
  })
  
  # obtain the CMPS score
  ood <- order(pos.df$cmps, decreasing = T)
  ordered.pos.df <- pos.df[ood, ]
  ordered.current.voter <- current.voter.list[ood]
  
  CMPS <- ordered.pos.df$cmps[1]
  if(CMPS != current.max) {
    stop("unexpected: current.max didn't find the max CMPS score")
  }
  
  pos.df <- ordered.pos.df
  congruent.pos.idx <- ceiling(sum(ordered.pos.df$cmps == CMPS)/2)
  
  if(congruent.pos.idx <= 0) {
    congruent.pos <- NA
    current.voter <- rep(FALSE, length(input.ccp))
  } else {
    congruent.pos <- ordered.pos.df$position[congruent.pos.idx]
    current.voter <- ordered.current.voter[[congruent.pos.idx]]
  }
  
  return(list(CMPS.score = CMPS, nseg = length(input.ccp), 
              congruent.pos = congruent.pos, congruent.seg = current.voter, 
              congruent.seg.idx = (1:length(input.ccp))[current.voter], pos.df = pos.df ))
}




#' Computes the CMPS score of a comparison between two bullet profiles/signatures
#' 
#' Compute the Congruent Matching Profile Segments (CMPS) score based on two bullet profiles/signatures.
#' The reference profile will be divided into consecutive, non-overlapping, basis segments of the same length.
#' Then the number of segments that are congruent matching will be found as the CMPS score. 
#' By default, `extract_feature_cmps` implements the algorithm with multi-peak inspection at three 
#' different segment scale levels. By setting `npeaks.set` as a single-length vector, users can switch to the algorithm 
#' with multi-peak inspection at the basis scale level only.
#'
#' @param x a numeric vector, vector of the reference bullet signature/profile that will be divided into basis segments
#' @param y a numeric vector, vector of the comparison bullet signature/profile
#' @param seg_length a positive integer, the length of a basis segment
#' @param Tx a positive integer, the tolerance zone is `+/- Tx`
#' @param npeaks.set a numeric vector, specify the number of peaks to be found at each segment scale level 
#' * If `length(npeaks.set) == 1`, the algorithm uses multi-peak inspection only at the basis scale level;
#' * If `length(npeaks.set) > 1`, the algorithm uses multi-peak inspection at 
#'    different segment scale levels. 
#' * By default, `npeaks.set = c(5,3,1)`. Including more segment scale levels will reduce the number of false positive results
#' @param include `NULL` or a vector of character strings indicating what additional information should be included in
#' the output of `extract_feature_cmps`. All possible options are: "nseg", "congruent.pos", "congruent.seg", 
#' "congruent.seg.idx", "pos.df", "ccp.list","segments", and "parameters". If one wants to include them all, one can use
#' `include = "full_result"`. By default, `include = NULL` and only the CMPS score is returned
#' @param outlength `NULL` or a numeric vector, specify the segment length of each level of the basis segment when the 
#' multi-segment lengths strategy is being used. If `outlength = NULL`, then the length of a basis segment will be doubled
#' at each segment level
#' 
#' @return a numeric value or a list
#' * if `include = NULL`, returns the CMPS score (a numeric value) only
#' * if `include = ` one or a vector of strings listed above:
#'     + `nseg`: number of basis segments
#'     + `congruent.seg`: a vector of boolean values. `TRUE` means this basis segment is a congruent matching profile segment (CMPS)
#'     + `congruent.seg.idx`: the indices of all CMPS
#'     + `pos.df`: a dataframe that includes positions of correlation peaks and the CMPS score of these positions 
#'     + `ccp.list`: a list of consistent correlation peaks of each basis segment. 
#'     + `segments`: a list of all basis segments
#'     + `parameters`: a list that stores all parameters used in the function call
#' 
#' @export
#' @importFrom assertthat assert_that
#'
#' @examples
#' library(tidyverse)
#' library(cmpsR)
#' 
#' data("bullets")
#' land2_3 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
#' land1_2 <- bullets$sigs[bullets$bulletland == "1-2"][[1]]
#' 
#' # compute cmps
#' 
#' # algorithm with multi-peak insepction at three different segment scale levels
#' cmps_with_multi_scale <- extract_feature_cmps(land2_3$sig, land1_2$sig, include = "full_result" )
#' 
#' # algorithm with multi-peak inspection at the basis scale level only
#' cmps_without_multi_scale <- extract_feature_cmps(land2_3$sig, land1_2$sig, 
#'                                                  npeaks.set = 5, include = "full_result" )
#' 
#' # Another example
#' library(tidyverse)
#' 
#' data("bullets")
#' 
#' lands <- unique(bullets$bulletland)
#' 
#' comparisons <- data.frame(expand.grid(land1 = lands[1:6], land2 = lands[7:12]),
#'                           stringsAsFactors = FALSE)
#' 
#' comparisons <- comparisons %>%
#'   left_join(bullets %>% select(bulletland, sig1=sigs),
#'             by = c("land1" = "bulletland")) %>%
#'   left_join(bullets %>% select(bulletland, sig2=sigs),
#'             by = c("land2" = "bulletland"))
#' 
#' comparisons <- comparisons %>% mutate(
#'   cmps = purrr::map2(sig1, sig2, .f = function(x, y) {
#'     extract_feature_cmps(x$sig, y$sig, include = "full_result")
#'   })
#' )
#' 
#' comparisons <- comparisons %>%
#'   mutate(
#'     cmps_score = sapply(comparisons$cmps, function(x) x$CMPS.score),
#'     cmps_nseg = sapply(comparisons$cmps, function(x) x$nseg)
#'   )
#'   
#' cp1 <- comparisons %>% select(land1, land2, cmps_score, cmps_nseg)
#' cp1  
#' 
#' @references 
#' Chen, Zhe, Wei Chu, Johannes A Soons, Robert M Thompson, John Song, 
#' and Xuezeng Zhao. 2019. “Fired Bullet Signature Correlation Using the 
#' Congruent Matching Profile Segments (CMPS) Method.” Forensic Science 
#' International, December, #109964. https://doi.org/10.1016/j.forsciint.2019.109964.
extract_feature_cmps <- function(x, y, seg_length = 50, Tx = 25, npeaks.set = c(5, 3, 1),
                                 include = NULL, outlength = NULL) {
  assert_that(
    is.numeric(x), is.numeric(y), is.numeric(seg_length), # is.numeric(seg_scale_max),
    is.numeric(Tx), is.numeric(npeaks.set),
    (is.null(include) | is.character(include)),
    (is.null(outlength) | is.numeric(outlength))
  )
  
  assert_that(
    Tx > 0, seg_length > 0, all(npeaks.set > 0)
  )
  
  outlength_ <- c()
  seg_levels <- length(npeaks.set)
  
  if(is.null(outlength)) {
    outlength_ <- sapply(1:seg_levels, function(x) 2^(x-1)) * seg_length
  } else {
    outlength_ <- rep_len(outlength, seg_levels)
  }
  
  segments <- get_segs(x, seg_length)
  nseg <- length(segments$segs)
  
  
  if (seg_levels == 1) {
    ccp.list <- lapply(1:nseg, function(nseg) {
      ccr <- get_ccr_peaks(y, segments, seg_outlength = outlength_[seg_levels], 
                           nseg = nseg, npeaks = npeaks.set[seg_levels])
      ccr$peaks.pos
      
    })
  } else if(seg_levels > 1) {
    
    
    ccp.list <- lapply(1:nseg, function(nseg) {
      
      if(all(is.na(segments$segs[nseg]))) {
        return(NULL)
      }
      
      ccr.list <- lapply(1:seg_levels, function(seg_level) {
        
        get_ccr_peaks(y, segments, seg_outlength = outlength_[seg_level], 
                      nseg = nseg, npeaks = npeaks.set[seg_level])

      })
      
      rr <- get_ccp(ccr.list, Tx = Tx)
      
      return(rr)
      
    })
  } else {
    stop("npeaks.set is invalid. Please provide at least one number.")
  }
  
  
  cmps <- get_CMPS(ccp.list, Tx = Tx)
  if(cmps$nseg != nseg) {
    stop("unexpected: number of obs of ccp.list is not equal to nseg")
  }
  
  parameters <- list(x=x, y=y, seg_length=seg_length, Tx=Tx,
                     npeaks.set=npeaks.set, include=include, outlength = outlength_)
  
  cmps$ccp.list <- ccp.list
  cmps$segments <- segments
  cmps$parameters <- parameters
  
  
  # toc()
  if(is.null(include)) {
    return(cmps$CMPS.score)
  } else {
    mm <- match.arg(include, c("nseg", "congruent.pos", "congruent.seg", "congruent.seg.idx", 
                               "pos.df", "ccp.list", "segments", "parameters", "full_result"),
                    several.ok = TRUE)
    if("full_result" %in% mm) {
      return(cmps)
    }
    
    mm <- c("CMPS.score", mm)
    return(cmps[mm])
  }
  
}



