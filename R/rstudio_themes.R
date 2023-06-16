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
#' @param apply_theme The theme to apply after install
#' @details This package comes with four RStudio editor themes that can be installed with [rstudio_install_certe_themes()]: "Certe Light", "Certe Light Non-Bold", "Certe Dark" and "Certe Dark Non-Bold".
#' 
#' Quickly apply the light theme with [rstudio_set_certe_light()] and the dark theme with [rstudio_set_certe_dark()] (they will be installed if needed).
#' @rdname rstudio_theme
#' @importFrom rstudioapi getThemeInfo getThemes applyTheme removeTheme convertTheme addTheme
#' @importFrom readr write_lines read_lines
#' @export
rstudio_install_certe_themes <- function(apply_theme = "Certe Light") {
  # Created the themes with:
  # https://tmtheme-editor.herokuapp.com/
  
  themes <- getThemes()[which(names(getThemes()) %unlike% "certe")]
  first_dark_theme <- names(which(vapply(FUN.VALUE = logical(1), themes, function(t) t$isDark)))[1]
  first_light_theme <- names(which(vapply(FUN.VALUE = logical(1), themes, function(t) !t$isDark)))[1]
  
  if (is_dark_theme()) {
    try(applyTheme(first_dark_theme), silent = TRUE)
  } else {
    try(applyTheme(first_light_theme), silent = TRUE)
  }
  
  # try to remove old versions first
  suppressWarnings(try(removeTheme("Certe Light"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe Dark"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe Light Non-Bold"), silent = TRUE))
  suppressWarnings(try(removeTheme("Certe Dark Non-Bold"), silent = TRUE))
  
  # convert tmTheme to rsTheme
  convertTheme(themePath = system.file("rstudio/certe_light.tmTheme", package = "certestyle"),
               outputLocation = system.file("rstudio/", package = "certestyle"),
               add = FALSE)
  light <- read_lines(system.file("rstudio/certe_light.rsTheme", package = "certestyle"))
  convertTheme(themePath = system.file("rstudio/certe_dark.tmTheme", package = "certestyle"),
               outputLocation = system.file("rstudio/", package = "certestyle"),
               add = FALSE)
  dark <- read_lines(system.file("rstudio/certe_dark.rsTheme", package = "certestyle"))
  
  # add some font features for Fira Code
  font_features <- c(".ace_editor {",
                     "  /* ss02 is for straight >= and ss06 is for less visible first backslash (for escaping) */",
                     '  font-feature-settings: "ss02", "ss06";',
                     "}",
                     # TODO will print like this :)
                     ".ace_comment.ace_keyword.ace_operator {",
                     "  color: red;", 
                     "  font-weight: bold;",
                     "}")
  light <- c(light, font_features)
  dark <- c(dark, font_features)
  
  # replace font-weight bold with font-weight 500 (semibold), since bold seems to displace the cursor position
  light_non_bold <- gsub("bold;", "normal;", light, fixed = TRUE)
  dark_non_bold <- gsub("bold;", "normal;", dark, fixed = TRUE)
  light_non_bold <- gsub("Certe Light", "Certe Light Non-Bold", light_non_bold, fixed = TRUE)
  dark_non_bold <- gsub("Certe Dark", "Certe Dark Non-Bold", dark_non_bold, fixed = TRUE)
  
  # update the theme files
  write_lines(x = c(light, font_features),
              file = system.file("rstudio/certe_light.rsTheme", package = "certestyle"),
              append = FALSE)
  write_lines(x = c(dark, font_features),
              file = system.file("rstudio/certe_dark.rsTheme", package = "certestyle"),
              append = FALSE)
  write_lines(x = c(light_non_bold, font_features),
              file = paste0(system.file("rstudio", package = "certestyle"), "/certe_light_nonbold.rsTheme"),
              append = FALSE)
  write_lines(x = c(dark_non_bold, font_features),
              file = paste0(system.file("rstudio", package = "certestyle"), "/certe_dark_nonbold.rsTheme"),
              append = FALSE)
  
  # install and apply based on previous choice of dark/light
  addTheme(system.file("rstudio/certe_light.rsTheme", package = "certestyle"), apply = FALSE)
  addTheme(system.file("rstudio/certe_dark.rsTheme", package = "certestyle"), apply = FALSE)
  addTheme(system.file("rstudio/certe_light_nonbold.rsTheme", package = "certestyle"), apply = FALSE)
  addTheme(system.file("rstudio/certe_dark_nonbold.rsTheme", package = "certestyle"), apply = FALSE)
  
  added <- tools::toTitleCase(names(getThemes()[which(names(getThemes()) %like% "certe")]))
  
  rstudio_apply_theme(themename = apply_theme)
  
  message("Certe themes installed: ", paste(added, collapse = ", "), "\n",
          "Current theme set to: ", getThemeInfo()$editor)
}

#' @importFrom rstudioapi applyTheme
rstudio_apply_theme <- function(themename) {
  tryCatch(applyTheme(themename),
           error = function(e) {
             rstudio_install_certe_themes(apply_theme = themename)
           })
}

#' @rdname rstudio_theme
#' @export
rstudio_set_certe_light <- function() {
  rstudio_apply_theme("Certe Light")
}

#' @rdname rstudio_theme
#' @export
rstudio_set_certe_dark <- function() {
  rstudio_apply_theme("Certe Dark")
}

#' @rdname rstudio_theme
#' @export
rstudio_set_certe_light_nonbold <- function() {
  rstudio_apply_theme("Certe Light Non-Bold")
}

#' @rdname rstudio_theme
#' @export
rstudio_set_certe_dark_nonbold <- function() {
  rstudio_apply_theme("Certe Dark Non-Bold")
}
