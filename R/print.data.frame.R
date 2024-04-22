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

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
tbl_sum.printed_dataframe <- function(x) {
  if (isTRUE(base::l10n_info()$`UTF-8`)) {
    cross <- "\u00d7"
  } else {
    cross <- "x"
  }
  dims <- paste(format(NROW(x), big.mark = ","), cross, format(NCOL(x), big.mark = ","))
  names(dims) <- paste("A", paste(rev(attributes(x)$original_classes), collapse = "/"))
  dims
}

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
#' @noRd
print.data.frame <- function(x, ...) {
  # print data.frames as tibbles, if `tibble` is installed
  # and option 'print.data.frame_as_tibble' is TRUE
  if ("tibble" %in% rownames(utils::installed.packages())) {
    msg <- FALSE
    if (!identical(rownames(x), as.character(seq_len(NROW(x))))) {
      msg <- TRUE
      x <- tibble::rownames_to_column(x)
    }
    classes <- class(x)
    tibble <- tibble::as_tibble(x)
    class(tibble) <- c("printed_dataframe", class(tibble))
    attr(tibble, "original_classes") <- classes
    print(tibble, ..., na.print = NA)
    if (isTRUE(msg)) {
      message("NOTE: Row names printed as first column.")
    }
  } else {
    base::print.data.frame(x, ...)
  }
}

# Make sure tibbles print without stripping characters, etc. ----

tibble_col_format2_r <- function(x, ...) {
  out <- trimws(format(x))
  chars <- max(nchar(out))
  out[is.na(x)] <- pillar::style_na(NA)
  pillar::new_pillar_shaft_simple(out, align = "right", width = chars)
}
tibble_col_format2_l <- function(x, ...) {
  out <- trimws(format(x))
  chars <- max(nchar(out))
  out[is.na(x)] <- pillar::style_na(NA)
  pillar::new_pillar_shaft_simple(out, align = "left", width = chars)
}

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
pillar_shaft.integer <- tibble_col_format2_r

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
pillar_shaft.double <- tibble_col_format2_r

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
pillar_shaft.numeric <- tibble_col_format2_r

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
pillar_shaft.character <- tibble_col_format2_l

# will be exported using s3_register if option `print.data.frame_as_tibble` is set
pillar_shaft.logical <- function(x, ...) {
  out <- trimws(format(x))
  out[x] <- font_green("TRUE")
  out[!x] <- font_red("FALSE")
  out[is.na(x)] <- pillar::style_subtle(NA)
  pillar::new_pillar_shaft_simple(out, width = 5, align = "left")
}
