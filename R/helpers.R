#' #' Compute the Sum of Squares Ratio
#'
#' @param score a numeric vector, scores
#' @param label a character vector, the label of each score
#' @param MS boolean, whether to compute the mean squares instead of the sum of squares. Default is FALSE
#'
#' @return the sum of squares ratio
#' @export
#' @importFrom assertthat assert_that
#'
#' @examples
#' score <- c(rnorm(100), rnorm(100, mean = 5))
#' label <- c(rep("a", 100), rep("b", 100))
#' compute_ss_ratio(score, label)
compute_ss_ratio <- function(score, label, MS=FALSE) {
  
  assert_that(
    is.numeric(score), length(score) == length(label)
  )
  
  score.split <- split(score, label)
  
  group.mean <- sapply(score.split, mean, na.rm=TRUE)
  group.count <- sapply(score.split, function(tt) sum(!is.na(tt)) )
  total.mean <- mean(score, na.rm = TRUE)
  
  k <- length(unique(label))
  n <- length(score)
  
  var.between <- sum((group.mean - total.mean)^2 * group.count)
  var.within <- sum(sapply(score.split, function(tt) {
    sum((tt - mean(tt, na.rm = TRUE))^2)
  })) 
  
  ss.ratio <- var.between / var.within
  if(MS) {
    ss.ratio <- ss.ratio * (n-k) / (k-1)
  }
  return(ss.ratio)
}


#' Obtain a list of all phases of a bullet-by-bullet comparison
#'
#' @param land1 (numeric) vector with land ids of bullet 1
#' @param land2 (numeric) vector with land ids of bullet 2
#' @param score numeric vector of scores to be summarized into a single number
#' @param addNA logical value. In case of missing lands, are scores set to 0 (addNA = FALSE) or set to NA (addNA = TRUE)
#'
#' @return a list of all phases
#' @export
#' 
#' @importFrom stats xtabs
#'
#' @examples
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
#' cp1 <- cp1 %>% mutate(
#'   land1idx = land1 %>% str_sub(-1, -1) %>% as.numeric(),
#'   land2idx = land2 %>% str_sub(-1, -1) %>% as.numeric()
#' )
#' 
#' with(cp1, {
#'   get_all_phases(land1idx, land2idx, cmps_score, addNA = TRUE)
#' })
get_all_phases <- function(land1, land2, score, addNA = FALSE)
{
  maxland <- max(land1, land2)
  fullframe <- data.frame(expand.grid(land1 = 1:maxland, land2 = 1:maxland))
  bcompare <- data.frame(land1, land2, score)
  fullframe <- fullframe %>% left_join(bcompare, by = c("land1", 
                                                        "land2"))
  fullframe <- fullframe %>% mutate(land1 = factor(land1, levels = 1:maxland), 
                                    land2 = factor(land2, levels = 1:maxland))
  matrix <- xtabs(score ~ land1 + land2, data = fullframe, 
                  addNA = addNA)/xtabs(~land1 + land2, data = fullframe, 
                                       addNA = addNA)
  matrix <- cbind(matrix, matrix)
  scores_list <- 1:maxland %>% lapply(FUN = function(i) {
    if (i == 1) {
      diag(matrix)
    }
    else {
      i <- i - 1
      diag(matrix[, -(1:i)])
    }
  })
  
  return(scores_list)
}


#' Compute a Statistic for the Foreground Phase and the Background Phases
#' 
#' Compute a statistic (for example, a mean) based on all matching comparisons (foreground phase) and the same statistic
#' based on all non-matching comparisons (background phases)
#'
#' @param scores_list a list of all phases
#' @param FUNC a function to be applied to both the foreground phase and the background phases
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds
#' @param both logical value. If `TRUE`, return the values of the `FUNC` for both the foreground phase and the background phases;
#' if `FALSE`, return their difference
#'
#' @return If `both = TRUE`, return the values of the statistic (calculated by `FUNC`) for both the foreground phase and the
#' background phases; if `both = FALSE`, return the difference
#' @export
#'
#' @examples
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
#' cp1 <- cp1 %>% mutate(
#'   land1idx = land1 %>% str_sub(-1, -1) %>% as.numeric(),
#'   land2idx = land2 %>% str_sub(-1, -1) %>% as.numeric()
#' )
#' 
#' phases <- with(cp1, {
#'   get_all_phases(land1idx, land2idx, cmps_score, addNA = TRUE)
#' })
#' 
#' compute_diff_phase(phases)
compute_diff_phase <- function(scores_list, FUNC = mean, na.rm = TRUE, both = FALSE)
{
  scores <- sapply(scores_list, FUNC, na.rm=na.rm)
  max.phase <- which.max(scores)
  result.match <- max(scores, na.rm=na.rm)
  result.nmatch <- scores_list[-max.phase] %>% unlist() %>% FUNC(na.rm=na.rm)
  if(both) {
    return(c(result.match, result.nmatch))
  } else {
    return(result.match - result.nmatch)
  }
}


#' Compute Different Metrics Based on Scores
#'
#' @param land1 (numeric) vector with land ids of bullet 1
#' @param land2 (numeric) vector with land ids of bullet 2
#' @param score numeric vector of scores to be summarized into a single number
#' @param addNA logical value. In case of missing lands, are scores set to 0 (addNA = FALSE) or set to NA (addNA = TRUE)
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds
#' @param include a character vector specifying which metrics to be included in the result; if `include = NULL`, including 
#' all metrics
#' @param out_names a character vector specifying the variable names of each metric; if `out_names = NULL`, using the default names
#'
#' @details 
#' By default, this helper function computes four metrics.
#' 
#' `diff`: the difference between the mean score of the foreground phase and the mean score of the background phases
#' `diff.med`: the difference between the median score of the foreground phase and the median score of the background phases
#' `max`: the max score
#' `maxbar`: the mean score of the foreground phase 
#' 
#' @return a data frame containing values of the metrics
#' @export
#'
#' @examples
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
#' cp1 <- cp1 %>% mutate(
#'   land1idx = land1 %>% str_sub(-1, -1) %>% as.numeric(),
#'   land2idx = land2 %>% str_sub(-1, -1) %>% as.numeric()
#' )
#' 
#' with(cp1, {
#'   compute_score_metrics(land1idx, land2idx, cmps_score)
#' })
compute_score_metrics <- function(land1, land2, score, 
                                  addNA = TRUE, na.rm = TRUE, include = NULL, out_names = NULL) {
  
  assert_that(
    is.numeric(land1), is.numeric(land2), is.numeric(score)
  )
  
  scores_list <- get_all_phases(land1, land2, score, addNA = addNA)
  tmp.diff <- compute_diff_phase(scores_list, mean, na.rm, both = TRUE)
  
  result <- data.frame(
    diff = tmp.diff[1] - tmp.diff[2],
    diff.med = compute_diff_phase(scores_list, median, na.rm = na.rm),
    max = max(score, na.rm = na.rm),
    maxbar = tmp.diff[1]
  )
  
  if(is.null(include)) {
    if(!is.null(out_names) && length(out_names) == ncol(result)) {
      names(result) <- out_names
    } else if (!is.null(out_names)) {
      warning("Warning: Fail to change the variable names in the result.")
    }
    return(result)
  } else {
    mm <- match.arg(include, c("diff", "diff.med", "max", "maxbar"), several.ok = TRUE)
    
    if(!is.null(out_names) && length(out_names) == length(mm)) {
      names(result[mm]) <- out_names
      return(result[out_names])
    } else if (!is.null(out_names)) {
      warning("Warning: Fail to change the variable names in the result.")
    }
    
    return(result[mm])
    
  }
}


#' Helper Function for Plotting the Distribution of a Metric
#'
#' @param cmps_metric a data frame containing values of the metric and the labels
#' @param metric string. Which metric to be plotted
#' @param scaled logical value. If `scaled = TRUE`, the values should be within the interval of `[0, 1]`
#' @param SSratio logical value. Whether to show the sum of squares ratio value
#' @param plot_density logical value. If `plot_density = TRUE`, the function plots
#' group density on the y-axis; if `plot_density = FALSE`, it plots the count of a certain bin.
#' @param ... other arguments for plotting: `breaks`, `binwidth`, and `subtitle`
#'
#' @return a ggplot object
#' @import assertthat 
#' @export
metric_plot_helper <- function(
  cmps_metric, metric, scaled = FALSE, SSratio = TRUE, plot_density = TRUE,
  ...
  ) {
  assert_that(
    has_name(cmps_metric, "type_truth")
  )  
  
  dots <- list(...)
  if(scaled) {
    if(is.null(dots$breaks)) {
      dots$breaks <- seq(0,1,0.05)
    }
    
    if(is.null(dots$binwidth)) {
      dots$binwidth <- 0.04
    }
    
  } else {
    if(is.null(dots$breaks)) {
      dots$breaks <- seq(0,24,1)
    }
    
    if(is.null(dots$binwidth)) {
      dots$binwidth <- 1
    }
  }
  
  p <- cmps_metric %>% ggplot() +
    geom_histogram(aes(
      x = .data[[metric]], y = if (plot_density) .data$..density.. else .data$..count..,
      fill = as.factor(.data$type_truth)), 
      binwidth = dots$binwidth) +
    labs(
      x = metric,
      y = if (plot_density) "observed proportion by group" else "count",
      fill = "Comparison Type",
      subtitle = dots$subtitle
    ) +
    scale_x_continuous(breaks = dots$breaks) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_fill_manual(values=c("darkorange", "darkgrey"))
  
  ss.ratio <- compute_ss_ratio(cmps_metric[[metric]], cmps_metric$type_truth, MS = FALSE)
  if(SSratio) {
    p <- p + 
      annotate(geom = "label", x = Inf, y = Inf, 
               label = paste("SS Ratio:", round(ss.ratio, 2)), 
               fill = "white", hjust = 1, vjust = 1)
  }
  
  return(p)
}


