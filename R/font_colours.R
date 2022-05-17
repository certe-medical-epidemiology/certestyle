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
#' @details Most colours are adapted based on the RStudio theme (dark/light).
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
  if (is_dark_theme()) {
    try_colour(..., colour = "white", collapse = collapse)
  } else {
    try_colour(..., colour = "black", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_blue <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "certeblauw2", collapse = collapse)
  } else {
    try_colour(..., colour = "certeblauw", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_green <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "certegroen2", collapse = collapse)
  } else {
    try_colour(..., colour = "certegroen", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_magenta <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "certeroze2", collapse = collapse)
  } else {
    try_colour(..., colour = "certeroze", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_red <- function(..., collapse = " ") {
  try_colour(..., colour = "red", collapse = collapse)
}

#' @rdname font_colours
#' @export
font_red_bg <- function(..., collapse = " ") {
  try_colour(..., colour = "red", collapse = collapse, background = TRUE)
}

#' @rdname font_colours
#' @export
font_white <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "black", collapse = collapse)
  } else {
    try_colour(..., colour = "white", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_yellow <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "certegeel", collapse = collapse)
  } else {
    try_colour(..., colour = "certegeel0", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_subtle <- function(..., collapse = " ") {
  if (is_dark_theme()) {
    try_colour(..., colour = "certe25", collapse = collapse)
  } else {
    try_colour(..., colour = "certe75", collapse = collapse)
  }
}

#' @rdname font_colours
#' @export
font_silver <- function(..., collapse = " ") {
  font_subtle(..., collapse = collapse)
}

#' @rdname font_colours
#' @export
font_grey <- function(..., collapse = " ") {
  try_colour(..., colour = "grey50", collapse = collapse)
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
#' @importFrom crayon make_style
try_colour <- function(..., colour = NULL, collapse = " ", background = FALSE, before = NULL, after = NULL) {
  txt <- paste0(unlist(list(...)), collapse = collapse)
  if (isTRUE(has_colour())) {
    if (is.null(before) || is.null(after)) {
      cols <- attributes(make_style(colourpicker(colour), bg = background))
      before <- cols$`_styles`[[1]]$open
      after <- cols$`_styles`[[1]]$close
    }
    if (is.null(collapse)) {
      paste0(before, txt, after, collapse = NULL)
    } else {
      paste0(before, txt, after, collapse = "")
    }
  } else {
    txt
  }
}
