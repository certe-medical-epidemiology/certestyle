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

test_that("colourpicker works", {
  expect_identical(colourpicker("viridis", 10),
                   colourpicker(viridisLite::viridis(10)))
  if (getRversion() >= 4) {
    expect_identical(grDevices::palette.colors(8, "R3"),
                     as.character(colourpicker("R3", 8)))
    expect_identical(grDevices::palette.colors(8, "R4"),
                     as.character(colourpicker("R4", 8)))
  }
  
  expect_identical(as.character(colourpicker(c("certeblauw", "red", "tan1", "#ffa", "FFAA00"))),
                   c("#01617E", "#FF0000", "#FFA54F", "#FFFFAA", "#FFAA00"))
  
  expect_identical(as.character(colourpicker("certe", 12)),
                   unname(c(certe.colours[1:6], certe.colours[13:18])))
  expect_identical(as.character(colourpicker("certe2", 12)),
                   unname(c(certe.colours[7:12], certe.colours[19:24])))
  expect_identical(as.character(colourpicker("certe3", 12)),
                   unname(c(certe.colours[13:18], certe.colours[25:30])))
  
  expect_identical(as.character(colourpicker("red")), "#FF0000")
  expect_identical(as.character(colourpicker("red", opacity = 0.5)), "#FF000080")
})
