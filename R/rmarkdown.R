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

#' Markdown Template Properties
#' 
#' These functions can be used in R Markdown documents.
#' @param user_id user ID at Certe, defaults to currently logged in user
#' @details For LaTeX, 4 logo files can be used to add to the resulting PDF file: `logocentre` (38x58 mm), `logoleft` (38x58 mm), `logoright` (38x58 mm) and `logofooter` (7 mm high). They must be set as YAML parameters in an R Markdown file header.
#' @name rmarkdown
#' @rdname rmarkdown
#' @export
rmarkdown_author <- function(user_id = NULL) {
  if (is.null(user_id)) {
    user_id <- Sys.info()["user"]
  }
  read_secret(paste("user", user_id, "fullname", sep = "."))
}

#' @rdname rmarkdown
#' @export
rmarkdown_department <- function() {
  read_secret("department.name")
}

#' @rdname rmarkdown
#' @param date Date to print in "d mmmm yyyy"
#' @export
rmarkdown_date <- function(date = Sys.Date()) {
  format2(date, "d mmmm yyyy")
}

#' @rdname rmarkdown
#' @param ... data set (and options) to pass on to [tbl_flextable()][certetoolbox::tbl_flextable()]
#' @export
rmarkdown_table <- function(...) {
  knitr::knit_print(certetoolbox::tbl_flextable(...))
}

#' @rdname rmarkdown
#' @param type defaults to "latex", must be "latex" or "word" (case-insensitive)
#' @export
rmarkdown_template <- function(type = "latex") {
  type <- tolower(type)[1]
  if (type == "latex") {
    template_file <- "certe.tex"
  } else if (type == "word") {
    template_file <- "certe.docx"
  } else {
    stop("invalid 'type' for rmarkdown_template_file(), must be 'latex' or 'word'")
  }
  out <- system.file(paste0("rmarkdown/latextemplate/", template_file), package = "certestyle")
  if (out == "") {
    stop("Template file '", template_file, "' not found in certestyle package", call. = FALSE)
  }
  out
}

#' @rdname rmarkdown
#' @param logo_type type of logo, must be "front" or "footer" (case-insensitive). For the LaTeX template, the front logo must be max 16x7 cm, and the footer logo must be max 16x0.7 cm.
#' @export
rmarkdown_logo <- function(logo_type = "front") {
  logo_file <- paste0("certe", tolower(logo_type)[1], ".pdf")
  out <- system.file(paste0("rmarkdown/latextemplate/", logo_file), package = "certestyle")
  if (out == "") {
    stop("Logo file '", logo_file, "' not found in certestyle package", call. = FALSE)
  }
  out
}
