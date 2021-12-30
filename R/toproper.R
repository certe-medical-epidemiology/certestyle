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

#' Proper Case for Text
#' 
#' @param text text to transform
#' @param every_word set captial to every word
#' @param ... unused, allows for backwards compatibility and future extension
#' @export
toproper <- function(text,
                     every_word = FALSE,
                     ...) {
  text <- as.character(text)
  if (isTRUE(every_word)) {
    text <- tools::toTitleCase(text)
  }
  if (grepl("^[a-z]+( |$)", text, ignore.case = FALSE)) {
    substr(text, 1, 1) <- toupper(substr(text, 1, 1))
  }
  text
}
