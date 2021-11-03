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

#' Certe Themes for RStudio
#' 
#' Install and apply RStudio syntax highlighting in Certe theme colours.
#' @details This package comes with two styles that can be installed with [install_rstudio_certe_themes()]: "Certe Light" and "Certe Dark". Needless to say, this function requires RStudio.
#' 
#' Quickly apply the light theme with [rstudio_certe_light()] and the dark theme with [rstudio_certe_dark()] (they will be installed if needed).
#' @rdname rstudio_theme
#' @importFrom rstudioapi getThemeInfo getThemes applyTheme removeTheme convertTheme addTheme
#' @importFrom readr write_lines
#' @export
rstudio_install_certe_themes <- function() {
  # Created the themes with:
  # https://tmtheme-editor.herokuapp.com/
  
  first_dark_theme <- names(which(sapply(getThemes(), function(t) t$isDark)))[1]
  first_light_theme <- names(which(sapply(getThemes(), function(t) !t$isDark)))[1]
  
  now_dark <- getThemeInfo()$dark
  if (now_dark) {
    try(applyTheme(first_dark_theme), silent = TRUE)
  } else {
    try(applyTheme(first_light_theme), silent = TRUE)
  }
  
  # try to remove old versions first
  suppressWarnings(try(removeTheme("Certe"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe donker"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe Light"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe Dark"), silent = TRUE))
  
  # convert tmTheme to rsTheme
  convertTheme(themePath = system.file("rstudio/certe_light.tmTheme", package = "certestyle"),
               outputLocation = system.file("rstudio/", package = "certestyle"),
               add = FALSE)
  convertTheme(themePath = system.file("rstudio/certe_dark.tmTheme", package = "certestyle"),
               outputLocation = system.file("rstudio/", package = "certestyle"),
               add = FALSE)
  
  # add some font features for Fira Code
  x <- c(".ace_editor {",
         "  /* ss02 is for straight >= en ss06 is for less visible first backslash (for escaping) */",
         '  font-feature-settings: "ss02", "ss06";',
         "}")
  write_lines(x = x,
              file = system.file("rstudio/certe_light.rsTheme", package = "certestyle"),
              append = TRUE)
  write_lines(x = x,
              file = system.file("rstudio/certe_dark.rsTheme", package = "certestyle"),
              append = TRUE)
  
  # install and apply based on previous choice of dark/light
  addTheme(system.file("rstudio/certe_light.rsTheme", package = "certestyle"), apply = !now_dark)
  addTheme(system.file("rstudio/certe_dark.rsTheme", package = "certestyle"), apply = now_dark)
  
  message("Certe themes installed and current theme set to: ", getThemeInfo()$editor)
}

#' @rdname rstudio_theme
#' @importFrom rstudioapi applyTheme
#' @export
rstudio_set_certe_light <- function() {
  tryCatch(applyTheme("Certe Light"),
           error = function(e) {
             install_rstudio_certe_themes()
             applyTheme("Certe Light")
           })
}

#' @rdname rstudio_theme
#' @importFrom rstudioapi applyTheme
#' @export
rstudio_set_certe_dark <- function() {
  tryCatch(rstudioapi::applyTheme("Certe Dark"),
           error = function(e) {
             install_rstudio_certe_themes()
             rstudioapi::applyTheme("Certe Dark")
           })
}
