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

test_that("format2 works", {
  dt <- Sys.Date()
  expect_identical(format2(dt, "dd-mm-yyyy"),
                   format(dt, "%d-%m-%Y"))
  
  tm <- Sys.time()
  expect_identical(format2(tm),
                   format(tm, "%H:%M:%S"))
  
  expect_identical(format2(0.123), "0,12")
  expect_identical(format2(0.123, percent = TRUE), "12,3%")
  
  expect_identical(class(format2_scientific(1)), "expression")
})
