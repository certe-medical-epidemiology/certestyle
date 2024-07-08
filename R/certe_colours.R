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

#' Certe Colour Vector
#' 
#' This is a character vector with all Certe colours. It is used by [colourpicker()].
#' @details The colours are:
#' 
#' ```{r, echo = FALSE, comment = "#>"}
#' certe.colours
#' ```
#' 
#' In RGB:
#' 
#' ```{r, echo = FALSE, comment = "#>"}
#' print(sapply(certe.colours, function(x) paste0("rgb(", paste0(as.character(col2rgb(x)), collapse = ", "), ")")), quote = FALSE)
#' ```
#' @export
"certe.colours"

# if these have to change, run `data-raw/new-colours.R`
certe.colours <- c(
  certeblauw = "#4A647D",
  certegroen = "#93984C",
  certeroze = "#B4527F",
  certegeel = "#E4D559",
  certelila = "#CEB9D6",
  certebruin = "#998961",
  certeblauw2 = "#69849C",
  certegroen2 = "#AEB278", # "#B8BB8B",
  certeroze2 = "#C581A0", # "#C794AC",
  certegeel2 = "#E4DD9C",
  certelila2 = "#DED3E2",
  certebruin2 = "#B9B09C",
  certeblauw3 = "#97AABB", # "#7A93A9",
  certegroen3 = "#C9CCA5",
  certeroze3 = "#D5ACBF",
  certegeel3 = "#ECE6B1",
  certelila3 = "#E6DDE9",
  certebruin3 = "#CAC3B2",
  certeblauw4 = "#C5D0DB",
  certegroen4 = "#DFE1C8",
  certeroze4 = "#E7CCD8",
  certegeel4 = "#F5F1CF",
  certelila4 = "#F0EAF2",
  certebruin4 = "#DFDBD0",
  certeblauw5 = "#E2E7EC",
  certegroen5 = "#EEEFE4",
  certeroze5 = "#F2E6EB",
  certegeel5 = "#F9F7E8",
  certelila5 = "#F7F4F8",
  certebruin5 = "#EEECE8",
  certeblauw6 = "#F6F7F8",
  certegroen6 = "#F9F9F6",
  certeroze6 = "#FAF7F8",
  certegeel6 = "#FCFBF8",
  certelila6 = "#FCFBFC",
  certebruin6 = "#F9F8F7",
  certeblauw0 = "#3A4D5D",
  certegroen0 = "#5A5D33",
  certeroze0 = "#7F3C5B",
  certegeel0 = "#D4C230",
  certelila0 = "#BEA5C7",
  certebruin0 = "#675D45"
)
