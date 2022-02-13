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

.onAttach <- function(...) {
  if (isTRUE(as.logical(getOption("print.data.frame_as_tibble", FALSE)))) {
    s3_register("base::print", "data.frame")
    s3_register("pillar::tbl_sum", "printed_dataframe")
    s3_register("pillar::pillar_shaft", "character")
    s3_register("pillar::pillar_shaft", "integer")
    s3_register("pillar::pillar_shaft", "double")
    s3_register("pillar::pillar_shaft", "numeric")
    s3_register("pillar::pillar_shaft", "logical")
  }
}
