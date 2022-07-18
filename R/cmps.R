#' Compute the CMPS score
#'
#' Compute the CMPS score from a list of positions of (consistent) correlation peaks.
#' @param input_ccp a list of positions for (consistent) correlation peaks
#' @param Tx integer, the tolerance zone is `+/- Tx`
#'
#' @return a list of six components:
#' * `CMPS_score`: computed CMPS score
#' * `nseg`: the number of basis segments
#' * `congruent_pos`: the congruent position that results in the CMPS score
#' * `congruent_seg`: a boolean vector of the congruent matching profile segments
#' * `congruent_seg_idx`: the index of the congruent matching profile segments
#' * `pos_df`: a dataframe that includes all positions and their corresponding CMPS score
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
#' npeaks_set <- c(5,3,1)
#' outlength <- c(50, 100, 200)
#' 
#' ccp_list <- lapply(1:nseg, function(nseg) {
#'  ccr_list <- lapply(1:seg_scale_max, function(seg_scale) {
#'    get_ccr_peaks(y, segments, seg_outlength = outlength[seg_scale], 
#'    nseg = nseg, npeaks = npeaks_set[seg_scale])
#'  })
#' 
#'  get_ccp(ccr_list, Tx = 25)
#' })
#' cmps <- get_CMPS(ccp_list, Tx = 25)
get_CMPS <- function(input_ccp, Tx = 25) {
  
  assert_that(is.numeric(Tx))
  
  tt <- unlist(input_ccp)
  tt <- sort(tt)
  
  if(is.null(tt)) { 
    
    return(list(CMPS_score = 0, nseg = length(input_ccp), 
                congruent_pos = NA, congruent_seg = NA, 
                congruent_seg_idx = NA, pos_df = NA ))
  }
  
  pos_df <- data.frame(position = tt)
  
  current_max <- 0
  current_voter_list <- list()
  
  for (i in 1:nrow(pos_df)) {
    ccp_count <- sapply(1:length(input_ccp), function(j) {
      # if any one of the peaks belongs to the interval 
      # centered at this position, count 1
      any(abs(pos_df$position[i] - input_ccp[[j]]) <= Tx)
    })
    
    tmp.sum <- sum(ccp_count)
    
    if(tmp.sum >= current_max) {
      current_max <- tmp.sum
    }
    
    current_voter_list[[i]] <- ccp_count
    
    pos_df$cmps[i] <- tmp.sum
  }
  
  # obtain the CMPS score
  ood <- order(pos_df$cmps, decreasing = T)
  ordered.pos_df <- pos_df[ood, ]
  ordered.current_voter <- current_voter_list[ood]
  
  CMPS <- ordered.pos_df$cmps[1]
  if(CMPS != current_max) {
    stop("unexpected: current_max didn't find the max CMPS score")
  }
  
  pos_df <- ordered.pos_df
  congruent_pos_idx <- ceiling(sum(ordered.pos_df$cmps == CMPS)/2)
  
  if(congruent_pos_idx <= 0) {
    congruent_pos <- NA
    current_voter <- rep(FALSE, length(input_ccp))
  } else {
    congruent_pos <- ordered.pos_df$position[congruent_pos_idx]
    current_voter <- ordered.current_voter[[congruent_pos_idx]]
  }
  
  return(list(CMPS_score = CMPS, nseg = length(input_ccp), 
              congruent_pos = congruent_pos, congruent_seg = current_voter, 
              congruent_seg_idx = (1:length(input_ccp))[current_voter], pos_df = pos_df ))
}




#' Computes the CMPS score of a comparison between two bullet profiles/signatures
#' 
#' Compute the Congruent Matching Profile Segments (CMPS) score based on two bullet profiles/signatures.
#' The reference profile will be divided into consecutive, non-overlapping, basis segments of the same length.
#' Then the number of segments that are congruent matching will be found as the CMPS score. 
#' By default, `extract_feature_cmps` implements the algorithm with multi-peak inspection at three 
#' different segment scale levels. By setting `npeaks_set` as a single-length vector, users can switch to the algorithm 
#' with multi-peak inspection at the basis scale level only.
#'
#' @param x a numeric vector, vector of the reference bullet signature/profile that will be divided into basis segments
#' @param y a numeric vector, vector of the comparison bullet signature/profile
#' @param seg_length a positive integer, the length of a basis segment
#' @param Tx a positive integer, the tolerance zone is `+/- Tx`
#' @param npeaks_set a numeric vector, specify the number of peaks to be found at each segment scale level 
#' * If `length(npeaks_set) == 1`, the algorithm uses multi-peak inspection only at the basis scale level;
#' * If `length(npeaks_set) > 1`, the algorithm uses multi-peak inspection at 
#'    different segment scale levels. 
#' * By default, `npeaks_set = c(5,3,1)`. Including more segment scale levels will reduce the number of false positive results
#' @param include `NULL` or a vector of character strings indicating what additional information should be included in
#' the output of `extract_feature_cmps`. All possible options are: "nseg", "congruent_pos", "congruent_seg", 
#' "congruent_seg_idx", "pos_df", "ccp_list","segments", and "parameters". If one wants to include them all, one can use
#' `include = "full_result"`. By default, `include = NULL` and only the CMPS score is returned
#' @param outlength `NULL` or a numeric vector, specify the segment length of each level of the basis segment when the 
#' multi-segment lengths strategy is being used. If `outlength = NULL`, then the length of a basis segment will be doubled
#' at each segment level
#' 
#' @return a numeric value or a list
#' * if `include = NULL`, returns the CMPS score (a numeric value) only
#' * if `include = ` one or a vector of strings listed above:
#'     + `nseg`: number of basis segments
#'     + `congruent_seg`: a vector of boolean values. `TRUE` means this basis segment is a congruent matching profile segment (CMPS)
#'     + `congruent_seg_idx`: the indices of all CMPS
#'     + `pos_df`: a dataframe that includes positions of correlation peaks and the CMPS score of these positions 
#'     + `ccp_list`: a list of consistent correlation peaks of each basis segment. 
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
#'                                                  npeaks_set = 5, include = "full_result" )
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
#'     cmps_score = sapply(comparisons$cmps, function(x) x$CMPS_score),
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
extract_feature_cmps <- function(x, y, seg_length = 50, Tx = 25, npeaks_set = c(5, 3, 1),
                                 include = NULL, outlength = NULL) {
  assert_that(
    is.numeric(x), is.numeric(y), is.numeric(seg_length), 
    is.numeric(Tx), is.numeric(npeaks_set),
    (is.null(include) | is.character(include)),
    (is.null(outlength) | is.numeric(outlength))
  )
  
  assert_that(
    Tx > 0, seg_length > 0, all(npeaks_set > 0)
  )
  
  outlength_ <- c()
  seg_levels <- length(npeaks_set)
  
  if(is.null(outlength)) {
    outlength_ <- sapply(1:seg_levels, function(x) 2^(x-1)) * seg_length
  } else {
    outlength_ <- rep_len(outlength, seg_levels)
  }
  
  segments <- get_segs(x, seg_length)
  nseg <- length(segments$segs)
  
  
  if (seg_levels == 1) {
    ccp_list <- lapply(1:nseg, function(nseg) {
      ccr <- get_ccr_peaks(y, segments, seg_outlength = outlength_[seg_levels], 
                           nseg = nseg, npeaks = npeaks_set[seg_levels])
      ccr$peaks_pos
      
    })
  } else if(seg_levels > 1) {
    
    
    ccp_list <- lapply(1:nseg, function(nseg) {
      
      if(all(is.na(segments$segs[nseg]))) {
        return(NULL)
      }
      
      ccr_list <- lapply(1:seg_levels, function(seg_level) {
        
        get_ccr_peaks(y, segments, seg_outlength = outlength_[seg_level], 
                      nseg = nseg, npeaks = npeaks_set[seg_level])

      })
      
      rr <- get_ccp(ccr_list, Tx = Tx)
      
      return(rr)
      
    })
  } else {
    stop("npeaks_set is invalid. Please provide at least one number.")
  }
  
  
  cmps <- get_CMPS(ccp_list, Tx = Tx)
  if(cmps$nseg != nseg) {
    stop("unexpected: number of obs of ccp_list is not equal to nseg")
  }
  
  parameters <- list(x=x, y=y, seg_length=seg_length, Tx=Tx,
                     npeaks_set=npeaks_set, include=include, outlength = outlength_)
  
  cmps$ccp_list <- ccp_list
  cmps$segments <- segments
  cmps$parameters <- parameters
  
  
  if(is.null(include)) {
    return(cmps$CMPS_score)
  } else {
    mm <- match.arg(include, c("nseg", "congruent_pos", "congruent_seg", "congruent_seg_idx", 
                               "pos_df", "ccp_list", "segments", "parameters", "full_result"),
                    several.ok = TRUE)
    if("full_result" %in% mm) {
      return(cmps)
    }
    
    mm <- c("CMPS_score", mm)
    return(cmps[mm])
  }
  
}




