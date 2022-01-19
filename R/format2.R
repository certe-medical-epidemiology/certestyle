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

#' Formatting with readable `format` settings and Dutch defaults
#'
#' Formatting with readable `format` settings and Dutch defaults
#' @param x vector of values
#' @param round number of decimals to round to
#' @param force_decimals force printing decimals, even with trailing zeroes
#' @param min_length minimal length of output, overwrites `force_decimals`
#' @param format format to use, can be set with human-readable text such as `"d mmmm yyyy"` or POSIX such as `"%e %B %Y"`
#' @param locale language to set for dates
#' @param decimal.mark,big.mark decimal and thousands limiters
#' @param percent logical to transform numeric to percentage (character)
#' @param ... arguments given to methods such as [format()]
#' @rdname format2
#' @export
#' @return [format2()] always returns a [character].
#' @examples
#' format2("2021-01-01")
#' format2("2021-01-01", "yyyy-qq")
#' 
#' format2(Sys.time(), "d mmmm yyyy HH:MM")
#' 
#' # content-aware of decimal use
#' format2(1024)
#' format2(c(1024, 0.123))
#' 
#' format2(2.1)
#' format2(2.1, force_decimals = TRUE) # since default is 2 decimals
#' 
#' p <- cleaner::as.percentage(0.123)
#' format2(p)
format2 <- function(x,...) {
  UseMethod("format2")
}

#' @rdname format2
#' @importFrom readr guess_parser
#' @export
format2.default <- function(x, ...) {
  if (isTRUE(list(...)$percent)) {
    format2(as.percentage(x), ...)
  } else {
    # all below have to be wrapped in tryCatch to also work for e.g. functions and calls
    if (tryCatch(guess_parser(x) == "date", error = function(e) FALSE)) {
      format2(as.Date(x), ...)
    } else if (tryCatch(guess_parser(x) == "datetime", error = function(e) FALSE)) {
      format2(as.POSIXct(gsub("([0-9])T([0-9])", "\\1 \\2", x)), ...)
    } else if (tryCatch(all(is.double2(x)), error = function(e) FALSE)) {
      format2(as.double2(x), ...)
    } else {
      # fall back to base R format(), e.g. for character
      format(x, ...)
    }
  }
}

#' @rdname format2
#' @importFrom cleaner as.percentage
#' @export
format2.numeric <- function(x,
                            round = ifelse(percent, 1, 2),
                            force_decimals = ifelse(percent, TRUE, FALSE),
                            decimal.mark = ",",
                            big.mark = ".",
                            min_length = 0,
                            percent = FALSE,
                            ...) {
  
  if (percent == TRUE) {
    format2(x = as.percentage(x),
            round = round,
            force_decimals = force_decimals,
            decimal.mark = decimal.mark,
            big.mark = big.mark)
  } else {
    if (length(x) == 0) {
      return(character())
    }
    
    if (min_length > 0) {
      if (force_decimals == TRUE) {
        warning('`force_decimals = TRUE` will be overwritten by `min_length = ', min_length, '`.')
      }
      x <- formatC(as.integer(x),
                   width = min_length,
                   flag = "0")
    } else {
      if (force_decimals == TRUE) {
        x <- formatC(
          round(as.double(x), digits = round),
          digits = round, # for latest R? ifelse(identical(round, 0), 1, round),
          big.mark = big.mark,
          decimal.mark = decimal.mark,
          format = "f"
        )
      } else {
        x <- format(
          round(as.double(x), digits = round),
          scientific = FALSE,
          big.mark = big.mark,
          decimal.mark = decimal.mark
        )
      }
    }
    x <- gsub("NA", NA, x, fixed = TRUE)
    x <- gsub(" NA", NA, x, fixed = TRUE)
    x <- trimws(x)
    x
  }
}

#' @rdname format2
#' @importFrom cleaner percentage
#' @export
format2.percentage <- function(x,
                               round = NULL,
                               force_decimals = TRUE,
                               decimal.mark = ",",
                               big.mark = ".",
                               ...) {
  
  if (length(x) == 0) {
    return(character())
  }
  x <- as.double(x) * 100
  out <- format2(x, round = guess_decimals(x, round, minimum = 0), force_decimals = force_decimals)
  out[!is.na(out)] <- paste0(out[!is.na(out)], "%")
  out
}

#' @rdname format2
#' @export
format2.percent <- function(...) {
  .Deprecated("format2.percentage()", package = "certestyle")
  format2.percentage(...)
}

#' @rdname format2
#' @export
format2.Date <- function(x,
                         format = "d mmmm yyyy",
                         locale = "nl",
                         ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @rdname format2
#' @export
format2.POSIXt <- function(x,
                           format = "yyyy-mm-dd HH:MM:SS",
                           locale = "nl",
                           ...) {
  coerce_datetime(x = x, format = format, locale = locale, ...)
}

#' @rdname format2
#' @export
format2.hms <- function(x,
                        format = "HH:MM:SS",
                        ...) {
  format2(as.POSIXct(x), format = format, ...)
}

#' @rdname format2
#' @export
format2.difftime <- function(x,
                             round = 2,
                             force_decimals = FALSE,
                             decimal.mark = ",",
                             big.mark = ".",
                             ...) {
  format2.numeric(x,
                  round = round,
                  force_decimals = force_decimals,
                  percent = FALSE,
                  decimal.mark = decimal.mark,
                  ...)
}

#' @importFrom lubridate month quarter
#' @importFrom cleaner format_datetime
coerce_datetime <- function(x, format, locale, ...) {
  
  format <- format_datetime(format)
  
  if (!is.null(locale)) {
    if (locale %like% "^[a-z]{2}$") {
      locale <- paste0(tolower(locale), "_", toupper(locale))
    }
    if (Sys.getlocale("LC_TIME") %like% ".UTF-8" & locale %unlike% ".UTF-8") {
      locale <- paste0(locale, ".UTF-8")
    }
    # exception for Dutch on Windows
    if (locale %like% "nl_NL" && tolower(.Platform$OS.type) == "windows") {
      locale <- "Dutch_Netherlands.1252"
    }
  } else {
    locale <- Sys.getlocale("LC_TIME")
  }
  
  if (format == "%B") {
    # same as format = "mmmm"
    return(month(x, label = TRUE, abbr = FALSE, locale = locale))
  } else if (format == "%b") {
    # same as format = "mmm"
    return(month(x, label = TRUE, abbr = TRUE, locale = locale))
  }
  
  if (Sys.getlocale("LC_TIME") %unlike% locale) {
    old_option <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", locale = locale)
  } else {
    old_option <- NULL
  }
  
  if (inherits(x, c("hms", "difftime", "POSIXlt"))) {
    if (all(x %like% '^[0-9]+:[0-9]+')) {
      x <- paste("1970-01-01", x)
    }
    df <- data.frame(dat = as.POSIXct(x), form = format(as.POSIXct(x), format), stringsAsFactors = FALSE)
  } else {
    df <- data.frame(dat = as.POSIXct(x), form = format(as.POSIXct(x), format), stringsAsFactors = FALSE)
  }
  
  # remove extra spaces
  df$form <- gsub(" +", " ", trimws(df$form))
  
  # replace quarters
  if (any(df$form %like% "(q|qq)")) {
    df$quarter <- quarter(df$dat)
    df$quarter[df$form %like% "qq"] <- paste0("Q", df$quarter[df$form %like% "qq"])
    df$form <- gsub(pattern = '(q|qq)+', replacement = df$quarter, x = df$form)
  }
  
  if (!is.null(old_option)) {
    tryCatch(Sys.setlocale("LC_TIME", locale = old_option),
             error = function(e) warning("Unable to reset original language when running: ",
                                         'Sys.setlocale("LC_TIME", locale = "', old_option, '")',
                                         call. = FALSE))
  }
  
  if (format == "unix") {
    as.double(df$form)
  } else {
    df$form
  }
}

#' @rdname format2
#' @details The [format2_scientific()] function returns an [expression] and can be used in `ggplot2` plots.
#' @export
#' @examples 
#' 
#' # use format2_scientific for scientific labels in plots:
#' if (require("certeplot2")) {
#' 
#'    # y axis without scientific notation
#'    plot2(mtcars,
#'          y = hp * 1000)
#'    
#'    # y axis with scientific notation
#'    plot2(mtcars,
#'          y = hp * 1000,
#'          y.labels = format2_scientific)
#'          
#' }
format2_scientific <- function(x, 
                               decimal.mark = ",",
                               ...) {
  # based on http://stackoverflow.com/a/24241954
  
  # turn into character string in scientific notation
  txt <- format(as.double(x), scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  txt <- gsub("^(.*)e", "'\\1'e", txt)
  # replace zero with an actual zero
  txt <- gsub("' *0'e[+]00", "0", txt)
  # remove + of exponent
  txt <- gsub("e+", "e", txt, fixed = TRUE)
  # turn the 'e+' into plotmath format
  txt <- gsub("e", "%*%10^", txt, fixed = TRUE)
  # replace decimal mark
  txt <- gsub(".", decimal.mark, txt, fixed = TRUE)
  # return this as an expression
  parse(text = txt)
}
