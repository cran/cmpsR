## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/vignette-",
  echo=FALSE,
  warning = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 5,
  out.width = "80%"
)

library(tidyverse)
theme_set(theme_bw())

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  install.packages("cmpsR")

## ---- eval=FALSE, echo=TRUE---------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("willju-wangqian/cmpsR")

## ---- echo=TRUE---------------------------------------------------------------
library(cmpsR)
data("bullets")

## ----signature_plot-----------------------------------------------------------
signatures <- bullets %>% unnest(sigs)
signatures <- signatures %>% mutate(
  bulletland = factor(bulletland, levels=c(paste(rep(c(1,2), each=6), c(1:6,2:6,1), sep="-")))
)
signatures %>% ggplot(aes(x = x/1000, y = sig)) + geom_line() + facet_wrap(~bulletland, ncol=6) +
  theme_bw() +
  xlab("Length in mm") +
  ylab("Relative height in micron")

## ---- echo=TRUE---------------------------------------------------------------
library(cmpsR)
data("bullets")

x <- bullets$sigs[bullets$bulletland == "2-3"][[1]]$sig
y <- bullets$sigs[bullets$bulletland == "1-2"][[1]]$sig

cmps <- extract_feature_cmps(x, y, include = "full_result")
cmps$CMPS_score

## ----plot_example, fig.cap="A KM Comparison, x and y", fig.align='center'-----
# knitr::include_graphics("man/figures/step0.png")
signatures %>% filter(bulletland %in% c("2-3", "1-2")) %>% 
  ggplot(aes(x = x/1000, y = sig)) + geom_line(aes(color = bulletland)) +
  theme_bw() +
  xlab("Length in mm") +
  ylab("Relative height in micron") +
  labs(color = "bullet land")

## ----plot_cut_x, fig.cap="Cut x into consecutive and non-overlapping basis segments of the same length. Only 4 basis segments are shown here", out.width="80%", fig.keep="hold", fig.align='center'----
segments <- get_segs(x, len = 50)
nseg <- length(segments$segs)
df <- data.frame(value = unlist(segments$segs),
                 segs = rep(1:nseg, each = 50),
                 index = unlist(segments$index))
df$segs_tag <- paste("seg", df$segs)
df.partx <- data.frame(value = unlist(segments$segs),
                       segs = 0,
                       index = unlist(segments$index),
                       segs_tag = "x")
df.party <- data.frame(value = y,
                       segs = 0,
                       index = 1:length(y),
                       segs_tag = "y")
cutt <- sapply(segments$index, function(idx) {idx[1]})
rbind(df.partx, df) %>% filter(segs <= 4) %>% 
  ggplot() +
  geom_line(aes(x = index, y = value)) +
  geom_vline(xintercept = cutt, color = "red") +
  facet_grid(segs_tag ~ .) +
  xlab("position") +
  ylab("signature") +
  ggtitle("Cut Comparison Signature x into Consecutive and Non-overlapping Basis Segments")

## ----plot_y_and_seg, fig.cap="y and 7th basis segment", out.width="80%", fig.keep="hold", fig.align='center'----
seg3 <- df %>% filter(segs == 7)
seg3$segs <- "segment 7"
df.y <- data.frame(value = y, segs = "y", index = 1:length(y))
df.y <- rbind(df.y, seg3[,-4])
df.y$segs <- factor(df.y$segs, levels = c("y", "segment 7"))
df.y %>% filter(!is.na(value)) %>% 
  ggplot() +
  geom_line(aes(x = index, y = value, color = segs)) +
  labs(x = "position", y = "signature", color = "label") +
  ggtitle("Reference Signature y and the 7th Segment") +
  scale_color_manual(values = c("black", "red"))

## ----plot_ccf_y_seg, fig.cap="the cross-correlation function (ccf) between y and segment 7", out.width="80%", fig.keep="hold", fig.align='center'----
ccrpeaks <- get_ccr_peaks(y, segments = segments, 50, nseg = 7, npeaks = 1)
df.ccf <- data.frame(value = ccrpeaks$ccr$ccf, index = ccrpeaks$adj_pos)

df.ccf %>% ggplot() +
  geom_line(aes(index, value)) + 
  geom_vline(xintercept = ccrpeaks$peaks_pos, color = "red") +
  xlab("position") +
  ylab("ccf") +
  ggtitle("CCF of y and the 7th Basis Segment")

## ----plot_x_itself, fig.cap="ideal case: compare x to itself. The highest peak has value 1 and is marked by the blue dot", out.width="80%", fig.keep="hold", fig.align='center'----
comp <- x
npeaks <- 1
seg_outlength <- 50

ccf.df <- do.call(rbind, lapply(6:12, function(nss) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments, seg_outlength = seg_outlength, nseg = nss, npeaks = npeaks)
  df.ccf <- data.frame(value = ccrpeaks$ccr$ccf, index = ccrpeaks$adj_pos)
  df.ccf$segs <- nss
  df.ccf
}))
peak.df <- do.call(rbind, lapply(6:12, function(nss) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments, seg_outlength = seg_outlength, nseg = nss, npeaks = npeaks)
  df.ccf <- data.frame(value = ccrpeaks$peaks_heights, index = ccrpeaks$peaks_pos)
  df.ccf$segs <- nss
  df.ccf
}))

ccf.df %>% 
  ggplot() +
  geom_line(aes(x = index, y = value)) +
  geom_point(data = peak.df, aes(x = index, y = value),
             color = "blue") +
  geom_vline(xintercept = 0, color = "red") +
  facet_grid(segs ~ .) +
  # ggtitle("Real Case: x compares to y") +
  ggtitle("Ideal Case: x Compares to Itself") +
  xlab("position") +
  ylab("ccf")

## ----plot_real_xy, fig.cap="real case: compare x to y. The 5 highest peaks are marked by the blue dots", out.width="80%", fig.keep="hold", fig.align='center'----
comp <- y
npeaks <- 5
seg_outlength <- 50

ccf.df <- do.call(rbind, lapply(6:12, function(nss) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments, seg_outlength = seg_outlength, nseg = nss, npeaks = npeaks)
  df.ccf <- data.frame(value = ccrpeaks$ccr$ccf, index = ccrpeaks$adj_pos)
  df.ccf$segs <- nss
  df.ccf
}))
peak.df <- do.call(rbind, lapply(6:12, function(nss) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments, seg_outlength = seg_outlength, nseg = nss, npeaks = npeaks)
  df.ccf <- data.frame(value = ccrpeaks$peaks_heights, index = ccrpeaks$peaks_pos)
  df.ccf$segs <- nss
  df.ccf
}))

ccf.df %>% 
  ggplot() +
  geom_line(aes(x = index, y = value)) +
  geom_point(data = peak.df, aes(x = index, y = value),
             color = "blue") +
  geom_vline(xintercept = -6, color = "red") +
  facet_grid(segs ~ .) +
  ggtitle("Real Case: x compares to y") +
  # ggtitle("Ideal Case: x compares to itself") +
  xlab("position") +
  ylab("ccf")

## ----plot_false_positive, fig.cap="Multi Segment Lengths Strategy - increasing the segment length could decrease the number of false positive peaks in ccf curves", out.width="80%", fig.keep="hold", fig.align='center'----
comp <- y
nseg <- 7
npeaks_set <- c(5, 3, 1)

multi.seg <- do.call(rbind, lapply(c(50, 100, 200), function(scale) {
  tt <- get_seg_scale(segments, nseg, out_length = scale)
  df.tmp <- data.frame(value = tt$aug_seg, index=tt$aug_idx, scale=paste("length of", scale))
}))

multi.seg$scale <- factor(multi.seg$scale, 
                          levels = c("length of 50", "length of 100", "length of 200"))

# rbind(data.frame(value = x, index = 1:length(x), scale = "x"), multi.seg) 
p1 <- multi.seg %>% filter(!is.na(value)) %>% 
  ggplot() +
  geom_line(aes(index, value)) +
  facet_grid(scale ~ .) + 
  ylab("signature") +
  xlab("position") +
  ggtitle("Segment 7 in 3 Different Scale Levels")

out_length <- c(50, 100, 200)

multi.df <- do.call(rbind, lapply(1:3, function(scale) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments,
                            seg_outlength = out_length[scale],
                            nseg = nseg, npeaks = npeaks_set[scale])
  df.tmp <- data.frame(value = ccrpeaks$ccr$ccf, index = ccrpeaks$adj_pos)
  df.tmp$scale <- paste("scale level", scale)
  df.tmp
}))
multi.peak.df <- do.call(rbind, lapply(1:3, function(scale) {
  ccrpeaks <- get_ccr_peaks(comp, segments = segments, seg_outlength = out_length[scale],
                            nseg = nseg, npeaks = npeaks_set[scale])
  df.tmp <- data.frame(value = ccrpeaks$peaks_heights, index = ccrpeaks$peaks_pos)
  df.tmp$scale <- paste("scale level", scale)
  df.tmp
}))
p2 <- multi.df %>% ggplot() +
  geom_line(aes(x=index, y=value)) +
  geom_point(data = multi.peak.df, aes(x = index, y = value),
           color = "blue") +
  geom_vline(xintercept = -7, color = "red") +
  facet_grid(scale ~ .) +
  scale_x_continuous(breaks=seq(-400,800,100)) +
  ylab("ccf") +
  xlab("position") +
  ggtitle("ccf of segment 7 and y in 3 different scales")

p1
p2

## ----plot_ccf_seg7, echo=TRUE-------------------------------------------------
cmps <- extract_feature_cmps(x, y, include = "full_result")
cmps_plot_list <- cmpsR::cmps_segment_plot(cmps, seg_idx = 7)
ggpubr::ggarrange(plotlist = unlist(cmps_plot_list, recursive = FALSE),
                  nrow = 3, ncol = 2)

## ---- echo=TRUE, eval=TRUE----------------------------------------------------
cmps <- extract_feature_cmps(x, y, seg_length = 50, Tx = 25, 
                     npeaks_set = c(5, 3, 1), include = "full_result")
cmps$CMPS_score

## ----plot_ccf_seg6, echo=TRUE, eval=TRUE--------------------------------------
cmps_plot_list <- cmpsR::cmps_segment_plot(cmps, seg_idx = 6)
ggpubr::ggarrange(plotlist = unlist(cmps_plot_list, recursive = FALSE),
                  nrow = 3, ncol = 2)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
land23 <- bullets$sigs[bullets$bulletland == "2-3"][[1]]
land13 <- bullets$sigs[bullets$bulletland == "1-3"][[1]]

cmps_knm <- extract_feature_cmps(land23$sig, land13$sig, seg_length = 50, Tx = 25, 
                     npeaks_set = c(5, 3, 1), include="full_result")
cmps_knm$CMPS_score

## ----eval = TRUE, echo=TRUE---------------------------------------------------
library(tidyverse)
library(cmpsR)

data("bullets")

lands <- unique(bullets$bulletland)

comparisons <- data.frame(expand.grid(land1 = lands[1:6], land2 = lands[7:12]),
                          stringsAsFactors = FALSE)

comparisons <- comparisons %>%
  left_join(bullets %>% select(bulletland, sig1=sigs),
            by = c("land1" = "bulletland")) %>%
  left_join(bullets %>% select(bulletland, sig2=sigs),
            by = c("land2" = "bulletland"))

comparisons <- comparisons %>% mutate(
  cmps = purrr::map2(sig1, sig2, .f = function(x, y) {
    extract_feature_cmps(x$sig, y$sig, include = "full_result")
  })
)

comparisons <- comparisons %>%
  mutate(
    cmps_score = sapply(comparisons$cmps, function(x) x$CMPS_score),
    cmps_nseg = sapply(comparisons$cmps, function(x) x$nseg)
  )
  
cp1 <- comparisons %>% select(land1, land2, cmps_score, cmps_nseg)
cp1  

## ----plot_all_pairwise--------------------------------------------------------
comparisons <- comparisons %>%
  mutate(
    bulletA = gsub("(\\d)-\\d", "\\1", land1),
    landA = gsub("\\d-(\\d)", "\\1", land1),
    bulletB = gsub("(\\d)-\\d", "\\1", land2),
    landB = gsub("\\d-(\\d)", "\\1", land2)
  )

dframe <- comparisons %>% select(-sig1, -sig2)

cc.idx <- c(6,7, 14, 21, 28, 35)
dframe$samesource <- FALSE
dframe$samesource[cc.idx] <- TRUE

dframe <- dframe %>% mutate(
  landA = paste0("L", landA),
  landB = paste0("L", landB),
  landB = factor(landB, levels = paste0("L", c(2:6,1))),
  bulletA = paste0("Bullet ", bulletA),
  bulletB = paste0("Bullet ", bulletB)
)

dframe %>% ggplot(aes(x = landA, y = landB, fill = cmps_score)) + 
  geom_tile() + 
  geom_tile(aes(colour="same land"), fill=NA, data = dframe %>% filter(samesource), size=1) + 
  scale_fill_gradient2("CMPS score", low = "gray80", high = "darkorange", midpoint = 6) + 
  scale_colour_manual("Source", values="darkorange") +
  facet_grid(bulletB ~ bulletA) + xlab("Bullet1 Lands") + 
  ylab("Bullet2 Lands") + 
  geom_text(aes(label=cmps_score)) +
  theme_bw() +
  theme(aspect.ratio = 1)

