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

#' `is.double()` for Comma-Decimal Input
#' 
#' This works like [is.double()] and [as.double()], but is vectorised and can also check (and transform) comma-decimal input such as `"0,1"`.
#' @param x vector of values
#' @param dec characters to be treated as comma
#' @rdname as.double2
#' @export
#' @examples 
#' is.double(0.1)
#' is.double("0.1")
#' is.double("0,1")
#' 
#' is.double2(0.1)
#' is.double2("0.1")
#' is.double2("0,1")
#' 
#' is.double(c(0.1, "0.1", "0,1"))
#' is.double2(c(0.1, "0.1", "0,1"))
#' 
#' as.double(c(0.1, "0.1", "0,1"))
#' as.double2(c(0.1, "0.1", "0,1"))
is.double2 <- function(x, dec = c(".", ",")) {
  suppressWarnings(as.double2(x = x, dec = dec)) %like% "^[0-9.]+$"
}

#' @rdname as.double2
#' @export
as.double2 <- function(x, dec = c(".", ",")) {
  as.double(gsub(paste0("[", concat(dec), "]"), ".", x))
}
