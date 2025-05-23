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
#' @details
#' Use q/qq/Q/QQ for quartiles, and t/tt/T/TT for four-month periods ('tertaal' in Dutch).
#' @rdname format2
#' @export
#' @return [format2()] always returns a [character].
#' @examples
#' format2("2021-01-01")
#' format2("2021-01-01", "yyyy-qq")
#' format2("2021-01-01", "yyyy-tt")
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
format2 <- function(x, ...) {
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
                            decimal.mark = dec_mark(),
                            big.mark = big_mark(),
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
        warning("`force_decimals = TRUE` will be overwritten by `min_length = ", min_length, "`.")
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
                               round = 1,
                               force_decimals = TRUE,
                               decimal.mark = dec_mark(),
                               big.mark = big_mark(),
                               ...) {
  
  if (length(x) == 0) {
    return(character())
  }
  # this will call cleaner:::format.percentage
  out <- trimws(format(x,
                digits = round,
                decimal.mark = decimal.mark,
                big.mark = big.mark))
  out[out == "NA%"] <- NA_character_
  out
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
                             decimal.mark = dec_mark(),
                             big.mark = big_mark(),
                             ...) {
  format2.numeric(x,
                  round = round,
                  force_decimals = force_decimals,
                  percent = FALSE,
                  decimal.mark = decimal.mark,
                  ...)
}

#' @rdname format2
#' @export
format2.object_size <- function(x,
                                round = 1,
                                decimal.mark = dec_mark(),
                                ...) {
  bytes <- x
  decimals <- round
  
  if ("certetoolbox" %in% utils::installed.packages()) {
    return(certetoolbox::size_humanreadable(bytes = bytes, decimals = decimals, decimal.mark = decimal.mark))
  }
  
  bytes <- as.double(bytes)
  # Adapted from:
  # http://jeffreysambells.com/2012/10/25/human-readable-filesize-php
  size <- c("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
  factor <- floor((nchar(formatC(bytes, format = "f", digits = 0)) - 1) / 3)
  factor[factor > length(size) - 1] <- length(size) - 1
  # added slight improvement; no decimals for B and kB:
  decimals_bak <- decimals[1]
  decimals <- rep(decimals[1], length(bytes))
  decimals[size[factor + 1] %in% c("B", "kB")] <- 0
  # but do set decimal for kB under 100 kB
  decimals[size[factor + 1] == "kB" & (bytes / 1024) < 100] <- decimals_bak
  # format
  out <- paste(sprintf(paste0("%.", decimals, "f"), bytes / (1024 ^ factor)), size[factor + 1])
  out <- trimws(gsub(".", decimal.mark, out, fixed = TRUE)) 
  out
}

#' @importFrom lubridate month quarter
#' @importFrom cleaner format_datetime
coerce_datetime <- function(x, format, locale, ...) {
  
  format <- format_datetime(format)
  
  if (!is.null(locale)) {
    if (locale %like% "^[a-z]{2}$") {
      locale <- paste0(tolower(locale), "_", toupper(locale))
    }
    if (Sys.getlocale("LC_TIME") %like% ".UTF-8" && locale %unlike% ".UTF-8") {
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
    if (all(x %like% "^[0-9]+:[0-9]+")) {
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
    df$form <- unlist(Map(gsub,
                          df$quarter,
                          df$form,
                          MoreArgs = list(pattern = "(q|qq)+"),
                          USE.NAMES = FALSE))
  }
  # replace tertile
  if (any(df$form %like% "(^t|tt)")) {
    tertiles <- rep(1:3, each = 4)
    df$tertile <- tertiles[month(df$dat, label = FALSE)]
    df$tertile[df$form %like% "tt"] <- paste0("T", df$tertile[df$form %like% "tt"])
    df$form <- unlist(Map(gsub,
                          df$tertile,
                          df$form,
                          MoreArgs = list(pattern = "(^t|tt)+"),
                          USE.NAMES = FALSE))
  }
  
  if (!is.null(old_option)) {
    tryCatch(Sys.setlocale("LC_TIME", locale = old_option),
             error = function(e) {
               warning("Unable to reset original language when running: ",
                       'Sys.setlocale("LC_TIME", locale = "', old_option, '")',
                       call. = FALSE)
             })
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
#' format2_scientific(c(12345, 12345678))
#' 
#' format2_scientific(c(12345, 12345678), round = 1)
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
                               round = 2,
                               decimal.mark = dec_mark(),
                               ...) {
  
  # turn into character string in scientific notation
  txt <- format(as.double(x), scientific = TRUE)
  out <- rep(NA_character_, length(x))
  
  out[!is.na(x)] <- paste0("'", 
                           gsub(".", decimal.mark, round(as.double(gsub("^(.*?)e.*", "\\1", txt[!is.na(x)])), digits = round), fixed = TRUE), 
                           "'%*%10^", 
                           as.double(gsub("^.*?e(.*)", "\\1", txt[!is.na(x)])))
  # remove leading zeroes
  out[!is.na(x) & out == "'0'%*%10^0"] <- "0"
  # and ones
  out[!is.na(x)] <- gsub("'1'%*%", "", out[!is.na(x)], fixed = TRUE)
  parse(text = out)
}
