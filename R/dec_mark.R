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

#' Use Decimal Comma?
#' 
#' These functions determine which characters the decimal mark and big mark should be that are used in the Certe \R package functions. They base the determination on the \R [locale user settings][Sys.getlocale()].
#' @details For [dec_mark()]: this returns a comma (`","`) on Dutch systems, and a full stop (`"."`) otherwise. If the [option][options()] `"dec_mark"` is set, that value will be used if it is either a comma or a full stop.
#' 
#' For [big_mark()]: this returns a full stop if [dec_mark()] returns a comma, and a space otherwise. If the [option][options()] `"big_mark"` is set, that value will be used if it is either a comma (`","`) or a full stop (`"."`) or a space (`" "`) or an empty character (`""`).
#' @export
#' @examples
#' # according the current user settings / OS language:
#' dec_mark()
#' big_mark()
#' 
#' options(dec_mark = ",")
#' dec_mark()
#' big_mark()
#' 
#' options(dec_mark = ".")
#' dec_mark()
#' big_mark()
#' 
#' options(big_mark = ",")
#' dec_mark()
#' big_mark()
#' 
#' # clean up
#' options(dec_mark = NULL, big_mark = NULL)
dec_mark <- function() {
  option_set <- getOption("dec_mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".")) {
      return(option_set)
    } else {
      warning("Option 'dec_mark' has an invalid value and was ignored. Use a \",\" or \".\".", call. = FALSE)
    }
  }
  
  locale <- Sys.getlocale(category = "LC_ALL")
  if (locale %like% "(nl_NL|Dutch|Netherlands)") {
    return(",")
  } else {
    return(".")
  }
}

#' @rdname dec_mark
#' @export
big_mark <- function() {
  option_set <- getOption("big_mark", default = NULL)
  if (!is.null(option_set)) {
    if (option_set %in% c(",", ".", "", " ")) {
      return(option_set)
    } else {
      warning("Option 'big_mark' has an invalid value and was ignored. Use a \",\" or \".\" or \" \" or \"\".", call. = FALSE)
    }
  }
  
  switch(dec_mark(),
         "," = ".",
         "." = " ")
}

#' @rdname dec_mark
#' @details The function [dec_mark_english()] is short for `options(dec_mark = ".", big_mark = " ")` and useful for using the Certe \R packages in English-based academic research. This function is session-specific, meaning that it must be set in every new \R session (which is intended).
#' @export
dec_mark_english <- function() {
  options(dec_mark = ".", big_mark = " ")
}
