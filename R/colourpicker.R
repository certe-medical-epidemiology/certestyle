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

viridisLite_colours <- c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")

#' Colours from \R, Certe, Viridis and More
#'
#' Colours from \R, Certe, viridis and more. The output prints in the console with the actual colours.
#' @param x colour or colour palette name. Certe colours will be used from the [certe.colours] object. Input can be:
#' * `"certe"`
#' * `"certe0"` to `"certe6"` (higher numbers give lighter colours)
#' * `"certeblauw"`, `"certegroen"`, `"certeroze"`, `"certegeel"`, `"certelila"`, or `"certezachtlila"` (or any of these followed by a 0 to 6)
#' * `"certe_rsi"` or `"certe_rsi2"` for certeroze/certegeel/certegroen (will **always** return length 3)
#' * One of the colourblind-safe `viridisLite` palettes: `r paste0('\n  - ``"', viridisLite_colours, '"``', collapse = "")`
#' * One of the built-in palettes in \R (currently \R `r paste(R.version$major, R.version$minor, sep = ".")`): `r paste0('\n  - ``"', c(if (getRversion() >= 4) grDevices::palette.pals() else character(0), "topo", "heatmap", "rainbow", "terrain", "greyscale", "grayscale"), '"``', collapse = "")`
#' * One of the `r length(colours())` built-in [colours()] in \R, such as `r paste0('``"', sort(sample(colours()[colours() %unlike% "^grey|gray"], 5)), '"``', collapse = ", ")`
#' @param length size of the vector to be returned
#' @param opacity amount of opacity (0 = solid, 1 = transparent)
#' @param ... not used at the moment
#' @details Certe colours will be chosen as divergent as possible if the required output length is not too high. For example:
#' 
#' * `x = "certe"` tries to only return the `"certe"` colours (`"certeblauw"`, `"certegroen"`, ...), the `"certe3"` colours (`"certeblauw3"`, `"certegroen3"`, ...) and the `"certe5"` colours (`"certeblauw5"`, `"certegroen5"`, ...)
#' * `x = "certe2"` tries to only return the regular `"certe2"` colours (`"certeblauw2"`, `"certegroen2"`, ...), the `"certe4"` colours (`"certeblauw4"`, `"certegroen4"`, ...) and the `"certe6"` colours (`"certeblauw6"`, `"certegroen6"`, ...)
#' * `x = "certe3"` tries to only return the `"certe3"` colours (`"certeblauw3"`, `"certegroen3"`, ...) and the `"certe5"` colours (`"certeblauw5"`, `"certegroen5"`, ...)
#' @return [character] vector in HTML format (i.e., `"#AABBCC"`) with new class `colourpicker`
#' @rdname colourpicker
#' @importFrom grDevices rainbow heat.colors terrain.colors topo.colors col2rgb colours grey.colors rgb
#' @importFrom viridisLite viridis
#' @export
#' @examples
#' colourpicker("certegroen")
#' colourpicker("certe", 5)
#' colourpicker(c("certeblauw", "red", "tan1", "#ffa", "FFAA00"))
#' 
#' par(mar = c(0.5, 2.5, 1.5, 0)) # set plot margins for below plots
#' 
#' # Certe colours
#' barplot(12:1,
#'         col = colourpicker("certe", 12),
#'         main = "'certe': uses 'certe' + 'certe3'")
#' barplot(12:1,
#'         col = colourpicker("certe2", 12),
#'         main = "'certe2': uses 'certe2' + 'certe4'")
#' barplot(12:1,
#'         col = colourpicker("certe3", 12),
#'         main = "'certe3': uses 'certe3' + 'certe5'")
#' 
#' # all colourblind-safe colour palettes from the famous viridisLite package
#' barplot(1:7,
#'         col = colourpicker("viridis", 7))
#' barplot(1:7,
#'         col = colourpicker("magma", 7))
colourpicker <- function(x, length = 1, opacity = 0, ...) {
  
  x <- tolower(x)
  opacity <- as.double(opacity)
  length <- as.double(length)
  
  if ((length(x) > 1 && length != 1) | (length > 1 && length(x) != 1)) {
    stop("Either the length of `x`, or `length` must be 1")
  }
  
  x_names <- names(x) # required for named colour vectors such as in ggplot2 and certeplot2
  x <- unname(x)
                    
  # NA: should become transparent white
  x_na <- is.na(x)
  x[x_na] <- "#FFFFFF"
  
  # "certe1" and "certeblauw1" should just be "certe" and "certeblauw"
  x[x %like% "^certe.*1$"] <- gsub("1", "", x[x %like% "^certe.*1$"])
  
  if (length(x) == 1) {
    
    if (x %like% "certe") {
      
      if (x == "certe") {
        certe_selection <- certe.colours[names(certe.colours) %unlike% "0"]
        if (length <= sum(names(certe_selection) %unlike% "[246]")) {
          # since we don't need so many colours, only keep e.g. certeblauw, certeblauw3, certeblauw5
          certe_selection <- certe_selection[names(certe_selection) %unlike% "[246]"]
        }
      } else if (x == "certe0") {
        # dark Certe colours
        certe_selection <- c(certe.colours[names(certe.colours) %like% "certe[a-z]+0"],
                             certe.colours[names(certe.colours) %like% "certe[a-z]+$"],
                             certe.colours[names(certe.colours) %like% "certe[a-z]+[246]"])
      } else if (x == "certe2") {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+[2-6]"]
        if (length <= sum(names(certe_selection) %unlike% "[35]")) {
          certe_selection <- certe_selection[names(certe_selection) %unlike% "[35]"]
        }
      } else if (x == "certe3") {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+[3-6]"]
        if (length <= sum(names(certe_selection) %unlike% "[46]")) {
          certe_selection <- certe_selection[names(certe_selection) %unlike% "[46]"]
        }
      } else if (x == "certe4") {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+[4-6]"]
        if (length <= sum(names(certe_selection) %unlike% "5")) {
          certe_selection <- certe_selection[names(certe_selection) %unlike% "5"]
        }
      } else if (x == "certe5") {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+[5-6]"]
      } else if (x == "certe6") {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+6"]
      } else if (x %in% names(certe.colours)) {
        certe_selection <- rep(certe.colours[names(certe.colours) == x], length = length)
      } else if (x == "certe_rsi") {
        certe_selection <- c(certe.colours[names(certe.colours) == "certeroze"],
                             certe.colours[names(certe.colours) == "certegeel"],
                             certe.colours[names(certe.colours) == "certegroen"])
        if (!length %in% c(1, 3)) {
          length <- 3
          warning("`length` will be forced to 3 when using 'certe_rsi'", call. = FALSE)
        }
      } else if (x == "certe_rsi2") {
        certe_selection <- c(certe.colours[names(certe.colours) == "certeroze2"],
                             certe.colours[names(certe.colours) == "certegeel2"],
                             certe.colours[names(certe.colours) == "certegroen2"])
        if (!length %in% c(1, 3)) {
          length <- 3
          warning("`length` will be forced to 3 when using 'certe_rsi2'", call. = FALSE)
        }
      } else {
        certe_selection <- certe.colours[names(certe.colours) %like% "certe[a-z]+6"]
      }
      x <- certe_selection[seq_len(min(length, length(certe_selection)))]
      
    } else if (x %in% viridisLite_colours) {
      x <- viridis(length, option = x)
      
    } else if (getRversion() >= 4 && x %in% tolower(grDevices::palette.pals())) {
      x <- grDevices::palette.colors(length, palette = x)
      
    } else if (x == "topo") {
      x <- topo.colors(length)
    } else if (x == "heatmap") {
      x <- heat.colors(length)
    } else if (x == "rainbow") {
      x <- rainbow(length)
    } else if (x == "terrain") {
      x <- terrain.colors(length)
    } else if (x %in% c("greyscale", "grayscale")) {
      x <- grey.colors(length)
    }
    
    if (length > 1 & length(x) == 1) {
      x <- rep(x, length = length)
    }
    
  } 
  
  # replace Certe colour names with HTML code
  if (any(x %in% names(certe.colours))) {
    x[x %in% names(certe.colours)] <- certe.colours[match(x[x %in% names(certe.colours)], names(certe.colours))]
  }
  
  # replace R colour names with HTML code
  if (any(x %in% colours())) {
    x[x %in% colours()] <- sapply(as.list(as.data.frame(col2rgb(x[x %in% colours()]))),
                                  function(rgb) rgb(red = rgb[1], green = rgb[2], blue = rgb[3], maxColorValue = 255))
  }
  
  # support #ffa -> #ffffaa
  shorts <- x %like% "^#?[0-F]{3}$"
  if (any(shorts)) {
    x[shorts] <- paste0(sapply(strsplit(x[shorts], "")[[1]],
                               function(x) ifelse(x == "#", "", paste0(x, x))),
                        collapse = "")
  }
  
  # support fffaa -> #fffaa
  missing_hastag <- x %like% "^[0-F]{6}$"
  if (any(missing_hastag)) {
    x[missing_hastag] <- paste0("#", x[missing_hastag])
  }
  
  # some colours add FF as opacity to HTML colour - remove them
  x[which(nchar(x) > 7 & x %like% "FF$")] <- substr(x[which(nchar(x) > 7 & x %like% "FF$")], 1, 7)
  
  invalid <- x %unlike% "^#[0-F]{6}$"
  if (any(invalid)) {
    warning("Invalid colours, replacing with grey: ", paste0(unique(x[invalid]), collapse = ", "), call. = FALSE)
    x[invalid] <- sapply(seq_len(length(invalid)), function(i) {
      int <- sample(c(38:217), 1, replace = FALSE)
      rgb(int, int, int, maxColorValue = 255)
    })
  }
  
  if (length > length(x)) {
    # misses some colours, so fill with greys that are 70-95% white
    warning("Missing ", length - length(x), " colours, filling with random light greys", call. = FALSE)
    x <- c(x[1:length(x)],
           grey.colors(length - length(x), start = 0.7, end = 0.95))
  }
  
  # so everything is now hexadecimal; paste alpha (opacity) to it
  if (opacity > 0 & any(nchar(x) == 7)) {
    opacity <- toupper(as.hexmode(round((1 - opacity) * 255)))
    if (nchar(opacity) == 1) {
      opacity <- paste0("0", opacity)
    }
    x[nchar(x) == 7] <- paste0(x[nchar(x) == 7], opacity)
  }
  
  # support NA - make them white and transparent
  x[x_na] <- "#FFFFFF00"
  
  x <- toupper(unname(x))
  names(x) <- x_names
  structure(x, class = c("colourpicker", "character"))
}

#' @method as.character colourpicker
#' @rdname colourpicker
#' @export
as.character.colourpicker <- function(x, ...) {
  substr(unclass(x), 1, 9)
}

#' @method print colourpicker
#' @importFrom crayon make_style
#' @rdname colourpicker
#' @export
print.colourpicker <- function(x, ...) {
  if (any(nchar(as.character(x)) == 9)) {
    # some have opacity as last 2 characters
    str_length <- 9
  } else {
    str_length <- 7
  }
  y <- x
  cols <- substr(y[!is.na(x)], 1, str_length)
  y[!is.na(x)] <- paste0('"', cols, '"',
                         sapply(cols, function(z) make_style(z, bg = TRUE)(" ")))
  max_print <- floor(options()$width / (str_length + 5)) + 1
  for (i in c(0:(length(y) / max_print))) {
    from <- i * max_print + 1
    to <- min(i * max_print + max_print, length(y))
    if (from <= length(y)) {
      ind <- seq(from = from, to = to, by = 1)
      cat(paste0("[",
                 formatC(from,
                         width = ifelse(length(y) <= max_print, 1, nchar(length(y)))),
                 "]"),
          y[ind], sep = " ")
      
      cat("\n")
    }
  }
  invisible(x)
}

