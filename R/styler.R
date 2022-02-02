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

#' Syntax Format According to Certe Coding Guidelines
#'
#' @description Use this styler for formatting code by setting this as an option:
#' 
#' `options(styler.addins_style_transformer = "certe_style_transformer()")`
#' 
#' Then use the keyboard shortcut of the `styler` package to apply the formatting.
#' @param ... arguments passed on to [styler::tidyverse_style()]
#' @details The Certe styler keeps the first argument of a function on the same line, puts all following arguments on a new line, and does not add another new line after the last argument. This makes these lines:
#'
#' ```
#' example_isolates %>% count(hospital_id, gender) %>% plot2(x.title = "Hospital", y.title = "Count", title = "Count isolates per hospital/gender")
#'
#' example_isolates %>%
#'   count(hospital_id, gender) %>%
#'   plot2(x.title = "Hospital", y.title = "Count", title = "Count isolates per hospital/gender")
#' ```
#'
#' Change into:
#'
#' ```
#' example_isolates %>%
#'   count(hospital_id,
#'         gender) %>%
#'   plot2(x.title = "Hospital",
#'         y.title = "Count",
#'         title = "Count isolates per hospital/gender")
#' ```
#' @importFrom styler tidyverse_style
#' @importFrom purrr some negate
#' @export
certe_style_transformer <- function(...) {
  certe_style <- tidyverse_style(...)

  # line breaks between *all* arguments if line breaks between *any*
  certe_style$line_break$set_linebreak_each_argument_if_multi_line <- function(pd) {
    if (!(any(pd$token == "','"))) {
      return(pd)
    }
    # does this expression contain expressions with linebreaks?
    has_children <- some(pd$child, negate(is.null))
    is_function_definition <- pd$token[1] == "FUNCTION"
    if (has_children && !is_function_definition) {
      children <- pd$child
      # don't count anything inside {} as internal linebreaks
      idx_pre_open_brace <- which(pd$token_after == "'{'")
      if (length(idx_pre_open_brace)) {
        children[idx_pre_open_brace + 1] <- NULL
      }
    }

    idx_comma <- which(pd$token == "','")
    idx_open_paren <- grep("'[[(]'", pd$token)
    idx_close_paren <- grep("'(]|\\))'", pd$token)
    pd[idx_comma + 1L, "lag_newlines"] <- 1L
    if (length(idx_open_paren)) {
      pd[idx_open_paren[1] + 1L, "lag_newlines"] <- 1L
    }
    if (length(idx_close_paren)) {
      pd[idx_close_paren[length(idx_close_paren)], "lag_newlines"] <- 1L
    }
    # pd$token_before is sometimes NA, create a new one (take lag):
    token_lag <- c(NA, pd$token[seq_len(length(pd$token) - 1)])
    # no new lines before first argument or after last argument
    pd[which(pd$token == "')'" | token_lag == "'('"), "lag_newlines"] <- 0L
    # indent the arguments: the first starts on the same line as the command, but
    # other arguments should be indented with the length of the initial command
    # minus 1 for the opening parenthesis
    pd[which(pd$lag_newlines == 1), "indent"] <- nchar(pd[1, "text", drop = TRUE]) - 1

    pd
  }

  # Function arguments on new lines, indented with 2 spaces
  certe_style$indention$update_indention_ref_fun_dec <- function(pd_nested) {
    if (pd_nested$token[1] == "FUNCTION" && nrow(pd_nested) > 4) {
      seq <- seq.int(3L, nrow(pd_nested) - 2L)
      pd_nested$indention_ref_pos_id[seq] <- 0L
      pd_nested$indent[seq] <- 2L
    }
    pd_nested
  }

  certe_style
}
