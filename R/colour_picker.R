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

#' Kleuren uit de huisstijl van Certe en meer
#'
#' Hiermee kunnen alle Certe-kleuren gebruikt worden, maar daarnaast ook de huisstijl van de RuG, het kleurenblindheid-veilige \code{viridis} en nog 6 andere continue kleurenpaletten. Druk op F1 voor een voorbeeldplaatje van alle beschikbare kleuren.
#' @rdname colourpicker
#' @param x Kleur. Moet een geldige kleur zijn uit \code{\link{colors}} (zoals \code{"black"}, \code{"red"}), een HTML-code (zoals \code{"#ffffaa"}, \code{"#ffa"}), een lege waarde (\code{NA} of \code{NULL}), of: \cr \cr
#'   \code{"certe"} \cr Huisstijlkleuren van \href{http://www.certe.nl}{Certe}. Deze kleuren kunnen ook als input gebruikt worden: \code{"certeblauw"}, \code{"certegroen"}, \code{"certeroze"}, \code{"certegeel"}, \code{"certelila"}, \code{"certezachtlila"}, \code{"certeblauw2"}, \code{"certegroen2"}, \code{"certeroze2"}, \code{"certegeel2"}, \code{"certelila2"}, \code{"certezachtlila2"}, \code{"certeblauw3"}, \code{"certegroen3"}, \code{"certeroze3"}, \code{"certegeel3"}, \code{"certelila3"} en \code{"certezachtlila3"}. De rest wordt aangevuld met grijswaarden. Gebruik \code{"certe2"} of \code{"certe3"} om direct de zachtere tinten te gebruiken. \strong{Met \code{extended_spectrum = TRUE} kunnen al deze waarden ook eindigen op 4, 5 en 6.}\cr \cr
#'   \code{"certe_rsi"} \cr De kleuren \code{"certeroze"}, \code{"certegeel"} en \code{"certegroen"}. \cr \cr
#'   \code{"certe_rsi2"} \cr De kleuren \code{"certeroze2"}, \code{"certegeel2"} en \code{"certegroen2"}. \cr \cr
#'   \code{"izore"} \cr Huisstijlkleuren van \href{http://www.izore.nl}{Izore}. Deze kleuren kunnen ook als input gebruikt worden: \code{"izorerood"}, \code{"izoregrijs"}, \code{"izoregrijs2"}. \cr \cr
#'   \code{"rsi"} \cr De kleuren van het palet \code{"RdYlGn"}; pastelkleuren van rood, geel en groen. \cr \cr
#'   \code{"rug"} of \code{"ug"} \cr Huisstijlkleuren van de \href{http://www.rug.nl}{Rijkuniversiteit Groningen}. Deze kleuren kunnen ook als input gebruikt worden: \code{"rugrood"}, \code{"rugblauw"}, \code{"rugpaars"}, \code{"rugdonkerblauw"}, \code{"ruggroen"}, \code{"rugbordeauxrood"}, \code{"ruggrijs"}, \code{"ruggoud"} en \code{"rugzilver"}. De rest wordt aangevuld met grijswaarden. \cr \cr
#'   \code{"viridis"} \cr Kleurenblindheid-veilig, zie verderop en \code{\link[viridisLite]{viridis}}. \cr \cr
#'   \code{"R"}, \code{"rainbow"} of \code{"regenboog"} \cr Standaardkleuren van R, zie \code{\link{rainbow}} \cr \cr
#'   \code{"heat"}, \code{"heatmap"} of \code{"hitte"} \cr Zie \code{\link{heat.colors}} \cr \cr
#'   \code{"terrain"} of \code{"terrein"} \cr Zie \code{\link{terrain.colors}} \cr \cr
#'   \code{"topo"} of \code{"geo"} \cr Zie \code{\link{topo.colors}} \cr \cr
#'   \code{"prev"} of \code{"prevalentie"} \cr \cr
#'   \code{"grijs"}, \code{"grijswaarden"}, \code{"greyscale"} of \code{"grayscale"} \cr \cr
#'   \code{"colourbrewer"} of \code{"colorbrewer"} \cr Gebaseerd op de 4 meest divergerende kleuren die kleurenblindveilig, printvriendelijk en kopieerveilig zijn volgens \emph{ColorBrewer} (die advies biedt voor cartografie): oranje, gelig, licht- en donkerblauw. Zie \href{http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4}{deze pagina van ColorBrewer 2.0}. \cr \cr
#'   \code{"ggplot"} \cr Kleuren die in de eerste versie van ggplot gebruikt werden als \code{Set1}. \cr \cr
#'   \code{"ggplot2"} \cr Kleuren die in ggplot2 gebruikt worden als \code{Set2}. \cr \cr
#' @param length Standaard is \code{1}. Aantal kleuren dat geretourneerd moet worden.
#' @param opacity Standaard is \code{0}. Transparantie, een waarde tussen 0-1.
#' @param extended_spectrum Standaard is \code{FALSE}. Alleen voor Certe/Izore/RuG: met \code{TRUE} wordt het spectrum opgerekt van 0-3 naar 0-6. Dat betekent dat:
#' \itemize{
#'   \item{\code{colourpicker("certeblauw")} is gelijk aan \cr \code{colourpicker("certeblauw", extended_spectrum = TRUE)}}
#'   \item{\code{colourpicker("certeblauw3")} is gelijk aan \cr \code{colourpicker("certeblauw6", extended_spectrum = TRUE)}}
#' }
#' @details
#' \if{html}{
#'
#' \out{<div style="text-align: center">}\figure{palette.png}{options: style="width:925px;max-width:95\%;"}\out{</div>}
#'
#' }
#' \if{latex}{
#'
#'     \out{\begin{center}}\figure{palette.png}\out{\end{center}}
#' }
#'
#' About \href{https://CRAN.R-project.org/package=viridis/vignettes/intro-to-viridis.html#introduction}{viridis}:
#'
#' These colour scales are designed to be:
#' \itemize{
#'   \item \strong{Colourful}, spanning as wide a palette as possible so as to make differences easy to see,
#'   \item \strong{Perceptually uniform}, meaning that values close to each other have similar-appearing colours and values far away from each other have more different-appearing colors, consistently across the range of values,
#'   \item \strong{Robust to colourblindness}, so that the above properties hold true for people with common forms of colourblindness, as well as in grey scale printing.
#' }
#' @keywords kleur aantal tint palette
#' @return RGB-kleur(en) in HTML-tekst, zoals \code{"#849A42"} voor \code{x = "certegroen"}.
#' @export
#' @examples
#' colourpicker("certegroen")
#'
#' # Certe-kleuren:
#' df <- tibble(
#'   certe0 = colourpicker("certe0", 6, extended_spectrum = TRUE),
#'   certe = colourpicker("certe", 6, extended_spectrum = TRUE),
#'   certe2 = colourpicker("certe2", 6, extended_spectrum = TRUE),
#'   certe3 = colourpicker("certe3", 6, extended_spectrum = TRUE),
#'   certe4 = colourpicker("certe4", 6, extended_spectrum = TRUE),
#'   certe5 = colourpicker("certe5", 6, extended_spectrum = TRUE),
#'   certe6 = colourpicker("certe6", 6, extended_spectrum = TRUE)) %>%
#'   pivot_longer(everything())
#' cols <- df$value
#' names(cols) <- df$value
#'
#' ggplot(df %>%
#'          mutate(name = fct_inorder(name), value = fct_inorder(value))) +
#'   geom_col(aes(x = name, y = 1, fill = value)) +
#'   scale_fill_manual(values = cols) +
#'   labs(title = paste("Certe-kleuren")) +
#'   theme_certe(legend.position = "none")
#'
#' \dontrun{
#'
#' ..., col = colourpicker("certeblauw", nrow(tbl)), ...
#' }
colourpicker <- function(x, length = 1, opacity = 0, extended_spectrum = FALSE) {
  
  x <- tolower(x)
  opacity <- as.double(opacity)
  length <- as.double(length)
  extended_spectrum <- as.logical(extended_spectrum)
  
  x_names <- names(x) # dit is nodig om named vectors te ondersteunen in plot2(colours = ...)
  x_bak <- x
  
  # ondersteuning voor 100% doorzichtige kleur bij NA en NULL
  x[is.null(x)] <- '#XXXXXX'
  x[is.na(x)] <- '#XXXXXX'
  
  x[x == 'ug'] <- 'rug'
  
  if (extended_spectrum == FALSE & any(x %like% "^[a-z]+[4-6]$")) {
    extended_spectrum <- TRUE
    if (any(x %like% "^[a-z]+3$")) {
      warning("`extended_spectrum` for colourpicker() forced to as TRUE for colour(s) ",
              paste(paste0("'", x, "'"), collapse = ", "),
              call. = FALSE)
    }
  }
  if (length > 6 & x[1] %like% "certe[0-9]?$") {
    extended_spectrum <- TRUE
  }
  
  colours.brewers <- c('certe',
                       'certe2',
                       'certe3',
                       'certe4',
                       'certe5',
                       'certe6',
                       'certe0',
                       'certe_rsi',
                       'certe_rsi2',
                       'izore',
                       'rsi',
                       'rug',
                       'viridis',
                       'r',
                       'R',
                       'rainbow',
                       'regenboog',
                       'heat',
                       'heatmap',
                       'hitte',
                       'terrain',
                       'terrein',
                       'topo',
                       'geo',
                       'prev',
                       'prevalentie',
                       'colourbrewer',
                       'colorbrewer',
                       'grijs',
                       'grijswaarden',
                       'greyscale',
                       'grayscale',
                       'ggplot',
                       'ggplot2')
  
  colour.list_all <- character(0)
  
  if (x[1] == 'viridis') {
    colour.list <- viridis::viridis(length)
    
  } else if (x[1] %in% c('r', 'R', 'rainbow', 'regenboog')) {
    colour.list <- rainbow(length)
    
  } else if (x[1] %in% c('heat', 'heatmap', 'hitte')) {
    colour.list <- heat.colors(length)
    
  } else if (x[1] %in% c('terrain', 'terrein')) {
    colour.list <- terrain.colors(length)
    
  } else if (x[1] %in% c('topo', 'geo')) {
    colour.list <- topo.colors(length)
    
  } else if (x[1] %in% c('prev', 'prevalentie')) {
    colour.list <- colorRampPalette(c(
      rgb(140, 255, 140, maxColorValue = 255), # lichtgroen
      rgb(40, 220, 40, maxColorValue = 255), # groen
      rgb(220, 110, 20, maxColorValue = 255), # oranje
      rgb(170, 0, 0, maxColorValue = 255)) # donkerrood
    )(length)
    
  } else if (x[1] %in% c('colourbrewer', 'colorbrewer')) {
    # http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4
    colour.list <- colorRampPalette(c(
      rgb(230, 97, 1, maxColorValue = 255),
      rgb(253, 184, 99, maxColorValue = 255),
      rgb(178, 171, 210, maxColorValue = 255),
      rgb(94, 60, 153, maxColorValue = 255))
    )(length)
    
  } else if (x[1] == 'certe_rsi') {
    colour.list <- colorRampPalette(c(
      colourpicker("certeroze"),
      colourpicker("certegeel"),
      colourpicker("certegroen"))
    )(length)
  } else if (x[1] == 'certe_rsi2') {
    colour.list <- colorRampPalette(c(
      colourpicker("certeroze2"),
      colourpicker("certegeel2"),
      colourpicker("certegroen2"))
    )(length)
  } else if (x[1] == 'rsi') {
    # van het palet "RdYlGn"
    colour.list <- colorRampPalette(
      RColorBrewer::brewer.pal(n = 3, name = "RdYlGn")
    )(length)
    
  } else if (x[1] %in% c('grijs', 'grijswaarden', 'greyscale', 'grayscale')) {
    colour.list <- gray.colors(length)
    
  } else if (x[1] == 'ggplot') {
    # herschreven van https://stackoverflow.com/a/8197703/4575331
    colour.list <- grDevices::hcl(h = seq(from = 15, to = 375, length = length + 1),
                                  l = 65,
                                  c = 100)[1:length]
    
  } else if (x[1] == 'ggplot2') {
    if (length > 8) {
      stop("The `ggplot2` 'Set2' palette only supports up to 8 different colours.")
    }
    colour.list <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
                     "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    colour.list <- colour.list[1:length]
    
  } else {
    
    colour.list <- list(
      certeblauw = c(1, 97, 126),
      certegroen = c(139, 153, 52),
      certeroze = c(224, 72, 131),
      certegeel = c(255, 228, 0),
      certelila = c(171, 121, 179),
      certezachtlila = c(214, 182, 214),
      izorerood = c(216, 51, 36),
      izoregrijs = c(170, 182, 200),
      rugrood = c(204, 0, 0),
      rugblauw = c(0, 156, 239),
      rugpaars = c(119, 45, 107),
      rugdonkerblauw = c(45, 0, 142),
      ruggroen = c(0, 153, 119),
      rugbordeauxrood = c(124, 33, 40),
      ruggrijs = c(102, 109, 112),
      ruggoud = c(132, 112, 64),
      rugzilver = c(142, 142, 139)
    )
    
    colour.list_all <- expand_colours(colour.list, extended_spectrum = TRUE)
    colour.list <- expand_colours(colour.list, extended_spectrum = extended_spectrum)
  }
  
  if (x[1] %in% colours.brewers) {
    if (x[1] %like% ('certe|izore|rug')) {
      if (x[1] == 'certe0') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*0"]
      } else if (x[1] == 'certe2') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*2"]
      } else if (x[1] == 'certe3') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*3"]
      } else if (x[1] == 'certe4') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*4"]
      } else if (x[1] == 'certe5') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*5"]
      } else if (x[1] == 'certe6') {
        colour.list <- colour.list[names(colour.list) %like% "certe.*6"]
      } else {
        colour.list.tbl <- tibble(kleur = names(colour.list),
                                  waarde = colour.list) %>%
          mutate(index = as.integer(ifelse(kleur %like% "[0-9]$", gsub(".*([0-9])$", "\\1", kleur), -1))) %>%
          filter(index != 0, kleur %like% paste0('^', x[1])) %>%
          mutate(kleur_fct = fct_inorder(kleur)) %>%
          arrange(index, kleur_fct)
        colour.list <- colour.list.tbl$waarde
        names(colour.list) <- colour.list.tbl$kleur
      }
      
      # RuG helder blauw eruit als het lengte meer dan 3 is
      if (length > 3) {
        colour.list <- colour.list[which(colour.list %>% names() != 'rugblauw')]
      }
      
      # certe aanvullen met lichtere Certe-kleuren, dus certe2 aanvullen met certe3, enz.
      certe_expand <- FALSE
      if (length > 6 & x[1] %like% "certe[0-9]?$") {
        certe_expand <- TRUE
        
        if (x[1] == "certe") {
          x[1] <- "certe1"
        }
        search_for_int <- clean_numeric(x[1])
        if (search_for_int %% 2 == 0) {
          ints_to_keep <- c(2, 4, 6)
        } else {
          ints_to_keep <- c(1, 3, 5)
        }
        nm <- names(colour.list_all)
        nm[nm %unlike% "[0-9]"] <- paste0(nm[nm %unlike% "[0-9]"], "1")
        names(colour.list_all) <- nm
        colour.list <- colour.list_all[names(colour.list_all) %like% "certe" &
                                         names(colour.list_all) %like% paste0(ints_to_keep, collapse = "|")]
        nm <- names(colour.list)
        nm[nm %unlike% "[0-9]"] <- paste0(nm[nm %unlike% "[0-9]"], "1")
        nm <- gsub("(.*)(.)$", "\\2\\1", nm)
        nm <- gsub("blauw", "A", nm)
        nm <- gsub("groen", "B", nm)
        nm <- gsub("roze", "C", nm)
        nm <- gsub("geel", "D", nm)
        nm <- gsub("zachtlila", "F", nm)
        nm <- gsub("lila", "E", nm)
        names(colour.list) <- nm
        
        colour.list <- colour.list[order(names(colour.list), colour.list)]
        # nog een zelfde keer alles erachter plakken
        colour.list <- c(colour.list, colour.list, colour.list, colour.list)
        
        first_in_colour.list <- suppressWarnings(min(which(names(colour.list) %like% search_for_int)))
        if (is.infinite(first_in_colour.list)) {
          first_in_colour.list <- 1
        }
        x <- unname(colour.list[c(first_in_colour.list:(first_in_colour.list + length))])
      }
      
      if (certe_expand == FALSE) {
        if (length > length(colour.list)) {
          # vult na alle voorgedefinieerde kleuren aan met grijstinten tussen 70-95% wit
          x <- c(colour.list[1:length(colour.list)],
                 gray.colors(length - length(colour.list), start = 0.7, end = 0.95)) %>%
            unname()
        } else {
          x <- colour.list[1:length] %>% unname()
        }
      }
    } else {
      x <- colour.list
    }
    
  } else {
    
    # voor alle rest
    for (i in 1:length(x)) {
      if (x[i] %like% '^[0-F]{3}$' |
          x[i] %like% '^[0-F]{6}$') {
        x[i] <- paste0('#', x[i])
      }
      # ondersteuning voor #ffa -> #ffffaa
      if (x[i] %like% '^#[0-F]{3}$') {
        xi.bak <- x[i]
        x[i] <- '#'
        for (h in 2:4) {
          x[i] <- paste0(x[i], rep((xi.bak %>% split.every.n(1))[h], 2) %>% concat())
        }
      }
      if ((x[i] %>% substr(1, 7)) %unlike% '^#[0-F]{6}$') {
        if (x[i] %in% names(colour.list)) {
          x[i] <- colour.list[x[i]]
        } else if (!x[i] %in% colours() & !x[i] == '#XXXXXX') {
          # geen geldige R-kleur
          warning('Colour not found: ', x[i], ' - picking random grey between 15-85% black.', call. = FALSE)
          # grey15 = rgb(38, 38, 38); grey85 = rgb(217, 217, 217)
          if (length(x) == 1) {
            col.random <- sample(c(38:217), length, replace = FALSE)
            x <- rgb(
              red = col.random,
              green = col.random,
              blue = col.random,
              maxColorValue = 255)
          } else {
            col.random <- sample(c(38:217), 1, replace = FALSE)
            x[i] <- rgb(
              red = col.random,
              green = col.random,
              blue = col.random,
              maxColorValue = 255)
          }
        } else if (!x[i] == '#XXXXXX') {
          rgb.list <- col2rgb(x[i])
          x[i] <- rgb(
            red = rgb.list[1],
            green = rgb.list[2],
            blue = rgb.list[3],
            maxColorValue = 255
          )
        }
      }
    }
    x <- x %>% rep(times = length)
  }
  
  # bij sommige kleurenspectra wordt FF als alpha toegevoegd; verwijderen
  x[which(x %>% nchar() > 7)] <- x %>% substr(1, 7)
  
  # is nu hexadecimaal; alpha erachter plakken
  if (!all(opacity == 0, na.rm = TRUE)) {
    x <- paste0(x,
                ((1 - opacity) * 255) %>%
                  round() %>%
                  as.hexmode() %>%
                  toupper() %>%
                  if_else(nchar(.) == 1,
                          paste0('0', .),
                          .))
  }
  
  # ondersteuning voor 100% doorzichtige kleur bij NA en NULL
  x[x %like% '#XXXXXX'] <- '#FFFFFF00'
  x[is.na(x_bak)] <- NA_character_
  
  x <- unname(x)
  names(x) <- x_names
  structure(x, class = c("colourpicker", "character"))
}

#' @noRd
#' @method as.character colourpicker
#' @export
as.character.colourpicker <- function(x) {
  x <- substr(x, 1, 7)
  class(x) <- "character"
  x
}

#' @noRd
#' @method print colourpicker
#' @export
print.colourpicker <- function(x, ...) {
  y <- sapply(x, function(y) {
    if (is.na(y)) {
      crayon::red("NA        ")
    } else {
      paste0('"', substr(y, 1, 7), '"', crayon::make_style(y, bg = TRUE)(" "))
    }
  })
  # elk item heeft 10 tekens: '"#FFE400" ' (laatste is de kleur) plus 1 spatie volgend op de kleur
  max_print <- floor(options()$width / 12) + 1
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

# voor uitbreiden van kleuren van donker naar licht
expand_colours <- function(colour.list, extended_spectrum) {
  
  lightness_values <- c(-0.30, 0.35, 0.50, 0.70, 0.85, 0.95)
  saturation_values <- c( 0.9, 0.80, 0.85, 0.90, 0.80, 0.6)
  if (extended_spectrum == FALSE) {
    lightness_values <- lightness_values[c(1, 3, 6)]
    saturation_values <- saturation_values[c(1, 3, 6)]
  }
  
  # HSL ophalen (hue, saturation, lightness); we gaan de L aanpassen (0 is zwart, 1 is wit)
  colour.list.hsl <- lapply(colour.list, function(x) rgb2hsl(x[1], x[2], x[3]))
  
  for (i in 1:length(colour.list)) {
    kleur <- gsub("(.*)[0-5]$", "\\1", paste0(names(colour.list)[i]))
    hsl <- colour.list.hsl[[kleur]]
    for (tint in c(0, 2:length(lightness_values))) {
      s_new <- case_when(tint == 0 ~ hsl[2] * saturation_values[1],
                         tint == 2 ~ hsl[2] * saturation_values[2],
                         tint == 3 ~ hsl[2] * saturation_values[3],
                         tint == 4 ~ hsl[2] * saturation_values[4],
                         tint == 5 ~ hsl[2] * saturation_values[5],
                         tint == 6 ~ hsl[2] * saturation_values[6],
                         TRUE ~ hsl[2]) # zoals 1 en elk ander getal
      if (kleur %like% "certeblauw") {
        if (tint == 0) {
          l_new <- hsl[3] + ((1 - hsl[3]) * -0.15)
        } else if (tint == 2) {
          l_new <- hsl[3] + ((1 - hsl[3]) * 0.20)
        } else if (tint == 3 & extended_spectrum == TRUE) {
          l_new <- hsl[3] + ((1 - hsl[3]) * 0.30)
        } else {
          l_new <- hsl[3] + ((1 - hsl[3]) * lightness_values[tint])
        }
      } else {
        # alles behalve certeblauw
        l_new <- case_when(tint == 0 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[1]),
                           tint == 2 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[2]),
                           tint == 3 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[3]),
                           tint == 4 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[4]),
                           tint == 5 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[5]),
                           tint == 6 ~ hsl[3] + ((1 - hsl[3]) * lightness_values[6]),
                           TRUE ~ hsl[3]) # zoals 1 en elk ander getal
      }
      colour.list.hsl[[length(colour.list.hsl) + 1]] <- c(hsl[1], s_new, l_new)
      names(colour.list.hsl)[length(colour.list.hsl)] <- paste0(kleur, tint)
    }
  }
  colour.list <- lapply(colour.list.hsl, function(x) hsl2rgb(x[1], x[2], x[3]))
  colour.list <- sapply(colour.list, function(x) rgb(x[1], x[2], x[3], maxColorValue = 255))
  colour.list
}

# onderstaande functies gejat van plotwidgets::rgb2hsl en plotwidgets::hsl2rgb
hsl2rgb <- function(h, s, l, a = NULL) {
  if (is.null(a)) {
    hsl <- as.matrix(c(h, s, l))
  } else {
    hsl <- as.matrix(c(h, s, l, a))
  }
  
  if (nrow(hsl) == 4) {
    alpha <- hsl[4, , drop = F]
    hsl <- hsl[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  H <- hsl[1, ]
  S <- hsl[2, ]
  L <- hsl[3, ]
  C <- (1 - abs(2 * L - 1)) * S
  X <- C * (1 - abs(((H/60)%%2) - 1))
  m <- L - C/2
  rgb <- matrix(0, ncol = ncol(hsl), nrow = 3)
  rownames(rgb) <- c("R", "G", "B")
  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)
  for (i in 1:6) {
    sel <- 60 * (i - 1) <= H & H < 60 * i
    kX <- iX[i]
    kC <- iC[i]
    rgb[kX, sel] <- X[sel]
    rgb[kC, sel] <- C[sel]
  }
  rgb <- rgb + rep(m, each = 3)
  rgb <- round(rgb * 255)
  if (!is.null(alpha))
    rgb <- rbind(rgb, alpha = alpha)
  as.double(rgb)
}

rgb2hsl <- function(r, g, b, a = NULL) {
  if (is.null(a)) {
    rgb <- as.matrix(c(r, g, b), )
  } else {
    rgb <- as.matrix(c(r, g, b, a))
  }
  
  if (nrow(rgb) == 4) {
    alpha <- rgb[4, , drop = F]
    rgb <- rgb[-4, , drop = F]
  }
  else {
    alpha <- NULL
  }
  rgb <- rgb/255
  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs + mins)/2
  S <- d/(1 - abs(2 * L - 1))
  sel <- d == 0
  S[sel] <- 0
  wmax <- apply(rgb, 2, which.max)
  H <- L
  HR <- (rgb[2, ] - rgb[3, ])/(maxs - mins)
  HG <- 2 + (rgb[3, ] - rgb[1, ])/(maxs - mins)
  HB <- 4 + (rgb[1, ] - rgb[2, ])/(maxs - mins)
  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]
  H <- (H * 60)%%360
  H[mins == maxs] <- 0
  as.double(rbind(H = H, S = S, L = L, alpha = alpha))
}
