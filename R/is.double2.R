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

#' `is.double()` for comma-decimal numbers
#' 
#' `is.double()` for comma-decimal numbers.
#' @param dec characters to be treated as comma
#' @param na.rm ignore empty values
#' @param ... currently not in use
#' @description This works like [is.double()] and [as.double()], but can also check (and transform) comma-decimal input such as `"0,1"`.
#' @rdname as.double2
#' @importFrom certetoolbox `%like%` concat
#' @export is.double2
is.double2 <- function(x, dec = c(".", ","), na.rm = TRUE, ...) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  } else if (length(x[is.na(x)]) > 0) {
    return(NA_real_)
  }
  
  x %like% paste0("^[0-9]+[", concat(dec), "][0-9]+$") | x %like% "^[0-9]+$"
  
}

#' @rdname double
#' @export
as.double2 <- function(x, ...) {
  as.double(gsub(",", ".", x))
}
