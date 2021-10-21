# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

# if the Certe colours change, run below function again and change these lines
# this will generate all certe2 to certe6 colours
update_certe_cols <- function() {
  expand_colours(
    list(
      certeblauw = c(1, 97, 126),
      certegroen = c(139, 153, 52),
      certeroze = c(224, 72, 131),
      certegeel = c(255, 228, 0),
      certelila = c(171, 121, 179),
      certezachtlila = c(214, 182, 214)
    ),
    extended_spectrum = TRUE)
}

# for expanding the colours from dark to light
#' @importFrom dplyr dplyr::case_when
expand_colours <- function(colour.list, extended_spectrum) {
  
  lightness_values <- c(-0.30, 0.35, 0.50, 0.70, 0.85, 0.95)
  saturation_values <- c( 0.9, 0.80, 0.85, 0.90, 0.80, 0.6)
  if (extended_spectrum == FALSE) {
    lightness_values <- lightness_values[c(1, 3, 6)]
    saturation_values <- saturation_values[c(1, 3, 6)]
  }
  
  # get HSL (hue, saturation, lightness); we will change the L (0 is black, 1 is white)
  colour.list.hsl <- lapply(colour.list, function(x) rgb2hsl(x[1], x[2], x[3]))
  
  for (i in 1:length(colour.list)) {
    df_col <- gsub("(.*)[0-5]$", "\\1", paste0(names(colour.list)[i]))
    hsl <- colour.list.hsl[[df_col]]
    for (tint in c(0, 2:length(lightness_values))) {
      s_new <- dplyr::case_when(tint == 0 ~ hsl[2] * saturation_values[1],
                                tint == 2 ~ hsl[2] * saturation_values[2],
                                tint == 3 ~ hsl[2] * saturation_values[3],
                                tint == 4 ~ hsl[2] * saturation_values[4],
                                tint == 5 ~ hsl[2] * saturation_values[5],
                                tint == 6 ~ hsl[2] * saturation_values[6],
                                TRUE ~ hsl[2])
      if (df_col %like% "certeblauw") {
        if (tint == 0) {
          l_new <- hsl[3] + ((1 - hsl[3]) * -0.15)
        } else if (tint == 2) {
          l_new <- hsl[3] + ((1 - hsl[3]) * 0.20)
        } else if (tint == 3 & extended_spectrum == TRUE) {
          l_new <- hsl[3] + ((1 - hsl[3]) * 0.30)
        } else {
          l_new <- hsl[3] + ((1 - hsl[3]) * lightness_values[tint])
        }
      } else {
        # all except certeblauw
        l_new <- dplyr::case_when(tint == 0 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[1]),
                                  tint == 2 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[2]),
                                  tint == 3 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[3]),
                                  tint == 4 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[4]),
                                  tint == 5 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[5]),
                                  tint == 6 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[6]),
                                  TRUE ~ hsl[3])
      }
      colour.list.hsl[[length(colour.list.hsl) + 1]] <- c(hsl[1], s_new, l_new)
      names(colour.list.hsl)[length(colour.list.hsl)] <- paste0(df_col, tint)
    }
  }
  colour.list <- lapply(colour.list.hsl, function(x) hsl2rgb(x[1], x[2], x[3]))
  colour.list <- sapply(colour.list, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  colour.list
}

# below from plotwidgets::rgb2hsl and plotwidgets::hsl2rgb under same license
hsl2rgb <- function(h, s, l, a = NULL) {
  if (is.null(a)) {
    hsl <- as.matrix(c(h, s, l))
  } else {
    hsl <- as.matrix(c(h, s, l, a))
  }
  
  if (nrow(hsl) == 4) {
    alpha <- hsl[4, , drop = F]
    hsl <- hsl[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  H <- hsl[1, ]
  S <- hsl[2, ]
  L <- hsl[3, ]
  C <- (1 - abs(2 * L - 1)) * S
  X <- C * (1 - abs(((H/60)%%2) - 1))
  m <- L - C/2
  rgb <- matrix(0, ncol = ncol(hsl), nrow = 3)
  rownames(rgb) <- c("R", "G", "B")
  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)
  for (i in 1:6) {
    sel <- 60 * (i - 1) <= H & H < 60 * i
    kX <- iX[i]
    kC <- iC[i]
    rgb[kX, sel] <- X[sel]
    rgb[kC, sel] <- C[sel]
  }
  rgb <- rgb + rep(m, each = 3)
  rgb <- round(rgb * 255)
  if (!is.null(alpha))
    rgb <- rbind(rgb, alpha = alpha)
  as.double(rgb)
}

rgb2hsl <- function(r, g, b, a = NULL) {
  if (is.null(a)) {
    rgb <- as.matrix(c(r, g, b), )
  } else {
    rgb <- as.matrix(c(r, g, b, a))
  }
  
  if (nrow(rgb) == 4) {
    alpha <- rgb[4, , drop = F]
    rgb <- rgb[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  rgb <- rgb/255
  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs + mins)/2
  S <- d/(1 - abs(2 * L - 1))
  sel <- d == 0
  S[sel] <- 0
  wmax <- apply(rgb, 2, which.max)
  H <- L
  HR <- (rgb[2, ] - rgb[3, ])/(maxs - mins)
  HG <- 2 + (rgb[3, ] - rgb[1, ])/(maxs - mins)
  HB <- 4 + (rgb[1, ] - rgb[2, ])/(maxs - mins)
  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]
  H <- (H * 60)%%360
  H[mins == maxs] <- 0
  as.double(rbind(H = H, S = S, L = L, alpha = alpha))
}
