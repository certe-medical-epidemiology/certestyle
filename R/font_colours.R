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

#' Console Font Colours
#' 
#' Add colours and font formatting to console text.
#' @param ... character (vector)
#' @param collapse character to separate the text elements. Use `collapse = NULL` to *not* collapse the input.
#' @rdname font_colours
#' @name font_colours
#' @export
#' @examples 
#' cat(font_black("TEXT TEST"), "\n")
#' cat(font_red("TEXT TEST"), "\n")
#' cat(font_green("TEXT TEST"), "\n")
#' cat(font_blue("TEXT TEST"), "\n")
#' cat(font_bold("TEXT TEST"), "\n")
#' cat(font_italic("TEXT TEST"), "\n")
#' cat(font_underline("TEXT TEST"), "\n")
#' cat(font_red_bg(font_white("TEXT TEST")), "\n")
font_black <- function(..., collapse = " ") {
  try_colour(..., before = "\033[38;5;232m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_blue <- function(..., collapse = " ") {
  try_colour(..., before = "\033[34m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_green <- function(..., collapse = " ") {
  try_colour(..., before = "\033[32m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_magenta <- function(..., collapse = " ") {
  try_colour(..., before = "\033[35m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_red <- function(..., collapse = " ") {
  try_colour(..., before = "\033[31m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_red_bg <- function(..., collapse = " ") {
  try_colour(..., before = "\033[41m", after = "\033[49m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_silver <- function(..., collapse = " ") {
  try_colour(..., before = "\033[90m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_white <- function(..., collapse = " ") {
  try_colour(..., before = "\033[37m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_yellow <- function(..., collapse = " ") {
  try_colour(..., before = "\033[33m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_subtle <- function(..., collapse = " ") {
  try_colour(..., before = "\033[38;5;246m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_grey <- function(..., collapse = " ") {
  try_colour(..., before = "\033[38;5;249m", after = "\033[39m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_bold <- function(..., collapse = " ") {
  try_colour(..., before = "\033[1m", after = "\033[22m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_italic <- function(..., collapse = " ") {
  try_colour(..., before = "\033[3m", after = "\033[23m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_underline <- function(..., collapse = " ") {
  try_colour(..., before = "\033[4m", after = "\033[24m", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_stripstyle <- function(...) {
  # from crayon:::ansi_regex
  gsub("(?:(?:\\x{001b}\\[)|\\x{009b})(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])|\\x{001b}[A-M]", "", c(...), perl = TRUE)
}

#' @importFrom crayon has_color
has_colour <- function() {
  # always disables colours on emacs
  if (Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != "") {
    # disable on emacs, which only supports 8 colours
    return(FALSE)
  }
  has_color()
}

# set colours if console has_colour()
try_colour <- function(..., before, after, collapse = " ") {
  txt <- paste0(unlist(list(...)), collapse = collapse)
  if (isTRUE(has_colour())) {
    if (is.null(collapse)) {
      paste0(before, txt, after, collapse = NULL)
    } else {
      paste0(before, txt, after, collapse = "")
    }
  } else {
    txt
  }
}
