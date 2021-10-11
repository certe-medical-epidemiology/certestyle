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
#' @param force.decimals force printing decimals, even with trailing zeroes
#' @param non.scientific force non-scientific notation
#' @param min.length minimal length of output, overwrites `force.decimals`
#' @param format format to use, can be set with human-readable text such as `"d mmmm yyyy"` or POSIX such as `"%e %B %Y"`
#' @exportMethod format2
#' @rdname format2
#' @export
format2 <- function(x,...) {
  UseMethod("format2")
}

#' @method format2 default
#' @rdname format2
#' @export format2.default
#' @export
format2.default <- function(x,
                            format = "d mmmm yyyy",
                            percent = FALSE,
                            round = ifelse(percent, 1, 2),
                            force.decimals = ifelse(percent, TRUE, FALSE),
                            decimal.mark = ",",
                            big.mark = ".",
                            ...) {
  if (percent == TRUE) {
    format2(as.percentage(x),
            round = round,
            force.decimals = force.decimals,
            decimal.mark = decimal.mark,
            big.mark = big.mark,
            ...)
  } else {
    if (identical(class(x), "NULL")) {
      format(x, ...)
    } else if (any(c("list", "formula", "expression", "matrix") %in% class(x))) {
      format(x, ...)
    } else if (readr::guess_parser(x) == "date") {
      format2(as.Date(x), format = format, ...)
    } else if (all(is.double2(x))) {
      format2.numeric(x,
                      round = round,
                      force.decimals = force.decimals,
                      decimal.mark = decimal.mark,
                      big.mark = big.mark,
                      ...)
    } else {
      format(x, digits = round, ...)
    }
  }
}

#' @method format2 percentage
#' @rdname format2
#' @importFrom cleaner percentage
#' @export
format2.percentage <- function(x,
                               round = NULL,
                               decimal.mark = ",",
                               big.mark = ".",
                               ...) {
  
  if (length(x) == 0) {
    return(character())
  }
  percentage(as.double(x),
             digits = round,
             decimal.mark = decimal.mark,
             big.mark = big.mark)
}

#' @method format2 percent
#' @rdname format2
#' @export
format2.percent <- format2.percentage

#' @method format2 POSIXct
#' @rdname format2
#' @export
format2.POSIXct <- function(x, format = "d mmmm yyyy", ...) {
  if (inherits(x, "POSIXt")) {
    # so it's a time object
    if (missing(format)) {
      format2.POSIXt(x, ...)
    } else {
      format2.POSIXt(x, format, ...)
    }
  } else {
    format2.Date(x, format, ...)
  }
}

#' @method format2 POSIXlt
#' @rdname format2
#' @export format2.POSIXlt
#' @export
format2.POSIXlt <- function(x, format = "d mmmm yyyy", ...) {
  format2.Date(x, format, ...)
}

#' @method format2 POSIXt
#' @rdname format2
#' @export format2.POSIXt
#' @export
format2.POSIXt <- function(x, format = "HH:MM:SS", ...) {
  format2.Date(x, format, ...)
}

#' @method format2 hms
#' @rdname format2
#' @export format2.hms
#' @export
format2.hms <- function(x,
                        format = "HH:MM:SS",
                        round = 2,
                        force.decimals = FALSE,
                        decimal.mark = ",",
                        big.mark = ".",
                        ...) {
  if (is.double2(x)) {
    format2.numeric(x,
                    round = round,
                    force.decimals = force.decimals,
                    percent = FALSE,
                    decimal.mark = decimal.mark,
                    ...)
  } else {
    format2.Date(as.POSIXct(x), format = format, ...)
  }
}

#' @method format2 difftime
#' @rdname format2
#' @export format2.difftime
#' @export
format2.difftime <- function(x,
                             round = 2,
                             force.decimals = FALSE,
                             decimal.mark = ",",
                             big.mark = ".",
                             ...) {
  format2.numeric(x,
                  round = round,
                  force.decimals = force.decimals,
                  percent = FALSE,
                  decimal.mark = decimal.mark,
                  ...)
}

#' @method format2 Date
#' @rdname format2
#' @export format2.Date
#' @export
format2.Date <- function(x, format = "d mmmm yyyy", ...) {
  
  format <- date_generic(format)
  # if (all(x %like% '^[0-9]+:[0-9]+')) {
  #   x <- paste("1970-01-01", x)
  # }
  
  locale <- list(...)$locale
  if (!is.null(locale)) {
    if (locale %like% "^[a-z]{2}$") {
      locale <- paste0(toupper(locale), "_", tolower(locale))
    }
  } else {
    locale <- Sys.getlocale("LC_TIME")
  }
  
  # bij alleen maanden een factor retourneren, zodat labels bij plot2() kloppen
  if (format == "%B") {
    # zelfde als format = "mmmm"
    return(month(x, label = TRUE, abbr = FALSE, locale = locale))
  } else if (format == "%b") {
    # zelfde als format = "mmm"
    return(month(x, label = TRUE, abbr = TRUE, locale = locale))
  }
  
  if (any(x %>% class() %in% c("hms", "difftime", "POSIXlt"))) {
    if (all(x %like% '^[0-9]+:[0-9]+')) {
      x <- paste("1970-01-01", x)
    }
    # df <- tibble(dat = as.POSIXlt(x), form = format(as.POSIXlt(x), format))
    df <- tibble(dat = as.POSIXct(x), form = format(as.POSIXct(x), format))
  } else {
    df <- tibble(dat = as.POSIXct(x), form = format(as.POSIXct(x), format))
  }
  
  # Voorloopspatie moet verwijderd worden
  df$form <- trimws(df$form, "left")
  df$form <- gsub(" +", " ", df$form)
  
  # kwartalen vervangen
  if (any(df$form %like% "(q|qq)")) {
    df$kwartaal <- quarter(df$dat)
    df$kwartaal[df$form %like% "qq"] <- paste0("Q", df$kwartaal[df$form %like% "qq"])
    df$form <- str_replace_all(df$form, '(q|qq)+', df$kwartaal)
  }
  
  if (format == "unix") {
    df$form %>% as.double()
  } else {
    df$form
  }
}

#' @method format2 numeric
#' @rdname format2
#' @export format2.numeric
#' @export
format2.numeric <- function(x,
                            round = ifelse(percent, 1, 2),
                            force.decimals = ifelse(percent, TRUE, FALSE),
                            non.scientific = FALSE,
                            decimal.mark = ",",
                            big.mark = ".",
                            min.length = 0,
                            percent = FALSE,
                            ...) {
  
  if (percent == TRUE) {
    format2.percentage(x = as.percentage(x),
                       round = round,
                       force.decimals = force.decimals,
                       decimal.mark = decimal.mark,
                       big.mark = big.mark)
  } else {
    label_wetenschappelijk <- function(l) {
      # bron: http://stackoverflow.com/a/24241954
      
      # turn in to character string in scientific notation
      l <- format(l, scientific = TRUE)
      # quote the part before the exponent to keep all the digits
      l <- gsub("^(.*)e", "'\\1'e", l)
      # verwijder de + bij de exponent
      l <- gsub("e+", "e", l, fixed = TRUE)
      # afronden op ingestelde aantalen decimalen
      #l <- gsub("^(.*)e", paste0('"', substr(l, 1, 2 + round), '"e'), l)
      # turn the 'e+' into plotmath format
      l <- gsub("e", "%*%10^", l)
      # punt vervangen door komma
      l <- gsub(".", decimal.mark, l, fixed = TRUE)
      # return this as an expression
      parse(text = l)
    }
    
    if (length(x) == 0) {
      return(character())
    }
    
    if (min.length > 0) {
      if (force.decimals == TRUE) {
        warning('`force.decimals = TRUE` will be overwritten by `min.length = ', min.length, '`.')
      }
      # if (!(x %>% identical(x %>% as.integer()))) {
      #   warning('`force.decimals = TRUE` overwritten by `min.length = ', min.length, '`; transforming to real numbers.')
      # }
      x <- formatC(x %>% as.integer(),
                   width = min.length,
                   flag = "0")
    } else {
      if (force.decimals == TRUE) {
        x <-
          formatC(
            round(as.double(x), digits = round),
            digits = round,
            big.mark = big.mark,
            decimal.mark = decimal.mark,
            format = "f"
          )
      } else {
        x <-
          format(
            round(as.double(x), round),
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

#' @method format percent
#' @export
#' @noRd
format.percent <- function(x,
                           round = 2,
                           force.decimals = TRUE,
                           ...) {
  format2.percentage(as.percentage(x), round, force.decimals, ...)
}

#' @importFrom cleaner format_datetime
#' @export
date_generic <- cleaner::format_datetime
