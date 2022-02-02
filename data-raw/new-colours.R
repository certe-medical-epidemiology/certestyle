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
expand_colours <- function(colour.list, extended_spectrum) {
  
  lightness_values <- c(-0.30, 0.35, 0.50, 0.70, 0.85, 0.95)
  saturation_values <- c( 0.9, 0.80, 0.85, 0.90, 0.80, 0.6)
  if (extended_spectrum == FALSE) {
    lightness_values <- lightness_values[c(1, 3, 6)]
    saturation_values <- saturation_values[c(1, 3, 6)]
  }
  
  # get HSL (hue, saturation, lightness); we will change the L (0 is black, 1 is white)
  colour.list.hsl <- lapply(colour.list, function(x) rgb2hsl(x[1], x[2], x[3]))
  
  for (i in seq_len(length(colour.list))) {
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
  colour.list <- vapply(FUN.VALUE = character(1), colour.list, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  colour.list
}

