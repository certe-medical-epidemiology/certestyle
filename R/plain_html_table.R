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

#' Format Data Set as HTML
#'
#' Formats a [data.frame] into HTML code, so it can be used in e.g. [`mail()`][certemail::mail()] or [`teams()`][certeprojects::teams()].
#' @param x a [data.frame]
#' @param max_col maximum number of columns to return
#' @export
#' @examples
#' plain_html_table(mtcars[1:2, 1:2])
plain_html_table <- function(x, max_col = Inf) {
  if (inherits(x, "freq")) {
    # format frequency tables (from the 'cleaner' package)
    x <- format(x)
    x$percent <- gsub(".", ",", x$percent, fixed = TRUE)
    x$cum_percent <- gsub(".", ",", x$cum_percent, fixed = TRUE)
    colnames(x) <- c("Item", "Count", "Percent", "Cum. Count", "Cum. Percent")
  } else {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  
  if (!all(rownames(x) == as.character(seq_len(nrow(x))))) {
    # add row names as columns
    cols <- colnames(x)
    x$` ` <- rownames(x)
    x <- x[, c(" ", cols), drop = FALSE]
  }
  
  if (ncol(x) > max_col) {
    # set maximum number of cols
    x <- x[, c(1:max_col), drop = FALSE]
  }
  
  # header
  head <- paste0("<thead>", paste0("<td><strong>", colnames(x), "</strong></td>", collapse = ""), "</thead>")
  
  # body
  body <- lapply(x, function(col) {
    if (is.numeric(col) | all(col %like% "[0-9]%")) {
      # numbers and percentages: align to the right
      paste0("<td align=\"right\">", format2(col), "</td>")
    } else {
      paste0("<td>", as.character(col), "</td>")
    }
  })
  body <- lapply(as.data.frame(t(as.data.frame(body))),
                 function(row) paste0("<tr>", paste0(row, collapse = ""), "</tr>"))
  body <- paste0(unlist(body), collapse = "")
  
  # add everything together
  paste0("<table>", head, body, "</table>")
}
