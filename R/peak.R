#' Identify peaks of a cross correlation curve
#' 
#' Given a comparison profile and a segment, `get_ccr_peaks` computes the 
#' cross correlation curve and finds peaks of the curve.
#' @param comp a nueric vector, vector of the bullet comparison profile
#' @param segments list with basis segments and their corresponding indices in the original profile, obtianed by `get_segs()`
#' @param seg_outlength length of the enlarged segment
#' @param nseg integer. `nseg` = 3: the third segment in `segments`
#' @param npeaks integer. the number of peaks to be identified.
#'
#' @return a list consisting of:
#' * `ccr`: the cross correlation curve
#' * `adj.pos`: indices of the curve
#' * `peaks.pos`: position of the identified peaks
#' * `peaks.heights`: the cross correlation value (height of the curve) of the peaks
#' @export
#' 
#' @importFrom assertthat assert_that
#'
#' @examples
#' data("bullets")
#' land2_3 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
#' land1_2 <- bullets$sigs[bullets$bulletland == "1-2"][[1]]
#' x <- land2_3$sig
#' y <- land1_2$sig
#' 
#' segments <- get_segs(x, len = 50)
#' 
#' # compute ccf based on y and segment 7 with scale 1, then identify 5 highest peaks
#' ccrpeaks <- get_ccr_peaks(y, segments = segments, seg_outlength = 50,
#'                           nseg = 7, npeaks = 5)
get_ccr_peaks <- function(comp, segments, seg_outlength, nseg = 1, npeaks = 5){

  assert_that(
    is.numeric(comp), is.numeric(seg_outlength), is.numeric(nseg), is.numeric(npeaks)
  )
  
  tt <- get_seg_scale(segments, nseg, out_length = seg_outlength)
  min.overlap <- min(
    length(tt$aug_seg[!is.na(tt$aug_seg)])*0.9, 
    round(length(comp[!is.na(comp)])*0.1)
  )
  
  ccr <- get_ccf4(comp, tt$aug_seg, min.overlap = min.overlap)
  tmp_pos <- tt$aug_idx[1]
  
  peaks <- local_max_cmps(ccr$ccf)
  
  od <- order(ccr$ccf[peaks], decreasing = TRUE)[1:npeaks]
  # adjust the position
  adj_pos <- ccr$lag - tmp_pos + 1
  
  peaks.heights <- ccr$ccf[peaks][od]
  peaks.pos <- adj_pos[peaks][od]
  
  return(list(ccr = ccr, adj.pos = adj_pos,
              peaks.pos = peaks.pos, peaks.heights = peaks.heights))
  
}

#' Identify at most one consistent correlation peak (ccp) 
#'
#' If multi segment lengths strategy is being used, at most one consistent correlation
#' peak (ccp) will be found for the corresponding basis segment. If the ccp cannot be identified, 
#' return `NULL`
#' @param ccr.list list, obtained by `get_ccr_peaks`
#' @param Tx integer, the tolerance zone is `+/- Tx`
#'
#' @return integer, the position of the ccp if it is identified; `NULL` otherwise. 
#' @export
#' @importFrom assertthat assert_that
#'
#' @examples
#' data("bullets")
#' land2_3 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
#' land1_2 <- bullets$sigs[bullets$bulletland == "1-2"][[1]]
#' x <- land2_3$sig
#' y <- land1_2$sig
#' 
#' segments <- get_segs(x, len = 50)
#' 
#' # identify the consistent correlation peak when ccf curves are computed
#' # based on y and segment 7 in 3 different scales;
#' # the number of peaks identified in each scale are 5, 3, and 1, respectively.
#' seg_scale_max <- 3
#' npeaks.set <- c(5,3,1)
#' outlength <- c(50, 100, 200)
#' 
#' ccr.list <- lapply(1:seg_scale_max, function(seg_scale) {
#'   get_ccr_peaks(y, segments, seg_outlength = outlength[seg_scale], nseg = 7, 
#'   npeaks = npeaks.set[seg_scale])
#' })
#' 
#' get_ccp(ccr.list, Tx = 25)
get_ccp <- function(ccr.list, Tx = 25){
  
  assert_that(is.numeric(Tx))
  
  # the number of different scales we have
  seg_level <- length(ccr.list)
  
  # the highest level has only one position, 
  # set it as a basis
  basis <- ccr.list[[seg_level]]$peaks.pos
  
  # if the entire basis segment is NA 
  if(length(basis) == 0 & all(is.na(ccr.list[[seg_level]]$ccr$ccf))) { return(NULL) }
  
  rr <- lapply(seq_along(basis), function(idx) {
    ccp <- lapply(1:(seg_level), function(level) {
      ck.tmp <- abs(ccr.list[[level]]$peaks.pos - basis[idx]) <= Tx
      if((all(!ck.tmp)) | (all(is.na(ck.tmp)))) { return(NULL) }
      else {
        return(1)
      }
    })
    if(length(unlist(ccp)) == seg_level) { 
      # if every level has a peak, we have a ccp
      return(basis[idx])
    } else {
      # else, return NULL
      return(NULL)
    }
  })
  return(unlist(rr))
  
}

#' find local maximums
#' @useDynLib cmpsR, .registration=TRUE
#' @param x numeric vector, the input sequence
#' @param find_max a numeric scalor, the function finds maximums if `find_max = 0`
#' finds minimums if overwise.
local_max_cmps <- function(x, find_max = 0) {
  if(all(is.na(x))) {
    return(NULL)
  }
  tmp.x <- x
  idx <- seq_along(tmp.x)
  notna.idx <- !is.na(tmp.x)
  peak.idx <- .Call(local_max_c, tmp.x[notna.idx], find_max)
  peaks <- idx[notna.idx][peak.idx]
  return(peaks)
}
