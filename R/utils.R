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

guess_decimals <- function(x, digits, minimum = 1, maximum = 3) {
  if (!is.null(digits)) {
    return(digits)
  }
  if (maximum < minimum) {
    maximum <- minimum
  }
  if (minimum > maximum) {
    minimum <- maximum
  }
  max_places <- max(unlist(lapply(strsplit(sub("0+$", "", 
                                               as.character(as.double(x))), ".", fixed = TRUE),
                                  function(y) ifelse(length(y) == 2, nchar(y[2]), 0))), na.rm = TRUE)
  max(min(max_places,
          maximum, na.rm = TRUE),
      minimum, na.rm = TRUE)
}
