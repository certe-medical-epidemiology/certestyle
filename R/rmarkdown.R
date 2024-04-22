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
#' @param logo_type type of logo, must be one of the files in /inst/rmarkdown/latextemplate of the certestyle package. For the LaTeX template, the front logo must be 16x7 cm, and the footer logo must be 16x0.7 cm.
#' @export
rmarkdown_logo <- function(logo_type = "certe") {
  if (logo_type == "front") {
    # legacy
    logo_type <- "certe"
  } else if (logo_type == "footer") {
    # legacy
    logo_type <- "certefooter"
  }
  logo_file <- paste0(tolower(logo_type[1]), ".pdf")
  logo_file <- gsub(".pdf.pdf", ".pdf", logo_file, fixed = TRUE)
  out <- system.file(paste0("rmarkdown/latextemplate/", logo_file), package = "certestyle")
  if (out == "") {
    stop("Logo file '", logo_file, "' not found in certestyle package", call. = FALSE)
  }
  out
}

#' Get Current Markdown Colour
#' 
#' This function determines the Certe theme colour currently used in a markdown document (Quarto or R Markdown), based on the YAML header.
#' @param default default colour
#' @details
#' It returns a Certe colour if one is set in the YAML header, and checks in this order:
#' 
#' 1. `colour-main`
#' 2. `colour-titlepage-titlebanner`
#' 3. `colour-titlepage-full`
#' 4. `colour-verticalbars`
#' 5. `colour-heading1`
#' 
#' If none is set, it returns the default setting, which is `"certeblauw"`.
#' 
#' This function is the default to set the theme for [`tbl_flextable()`][certetoolbox::tbl_flextable()].
#' 
#' It can be also be used for [`plot2()`][certeplot2::plot2()]: 
#' 
#' ```
#' # will turn e.g. 'certeblauw' or 'certeroze' based on PDF format settings
#' library(certeplot2)
#' admitted_patients |>
#'   plot2(colour = current_markdown_colour())
#' ```
#' @export
current_markdown_colour <- function(default = "certeblauw") {
  params <- rmarkdown::metadata
  convert_to_certe_colour <- function(col) {
    if (grepl("certe", col)) {
      return(gsub(".*(certe[a-z]+).*", "\\1", col))
    } else {
      if (!grepl("^#", col)) {
        col <- paste0("#", col)
      }
      # check 'certe.colours' (a certestyle object)
      out <- names(certe.colours[certe.colours == col])
      if (length(out) == 0 || !grepl("certe", out)) {
        return("")
      } else {
        # return without numbers, so "certegroen" instead of "certegroen3"
        return(gsub("[^a-z]", "", out))
      }
    }
  }
  if ("colour-main" %in% names(params) && grepl("certe", convert_to_certe_colour(params$`colour-main`))) {
    # take main colour if set
    convert_to_certe_colour(params$`colour-main`)
  } else if ("colour-titlepage-titlebanner" %in% names(params) && grepl("certe", convert_to_certe_colour(params$`colour-titlepage-titlebanner`))) {
    # take vertical bars colour if set
    convert_to_certe_colour(params$`colour-titlepage-titlebanner`)
  } else if ("colour-titlepage-full" %in% names(params) && grepl("certe", convert_to_certe_colour(params$`colour-titlepage-full`))) {
    # take vertical bars colour if set
    convert_to_certe_colour(params$`colour-titlepage-full`)
  } else if ("colour-verticalbars" %in% names(params) && grepl("certe", convert_to_certe_colour(params$`colour-verticalbars`))) {
    # take vertical bars colour if set
    convert_to_certe_colour(params$`colour-verticalbars`)
  } else if ("colour-heading1" %in% names(params) && grepl("certe", convert_to_certe_colour(params$`colour-heading1`))) {
    # take heading 1 colour if set
    convert_to_certe_colour(params$`colour-heading1`)
  } else {
    default
  }
}
