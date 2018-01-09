#' ---
#' title: "Analysis"
#' output:
#'   html_document:
#'     css: www/styles-ercot-minimal.css
#'     toc: true
#' ---
#'
#'
#+ include = FALSE
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

#'
#'
#'
#+ results = "asis", fig.show = "asis"
# knitr::image_uri("R/www/logo.png")
# file.exists("R/www/logo.png")
htmltools::img(
  src = knitr::image_uri("R/www/logo.png"),
  alt = 'logo',
  style = 'position:absolute;top:0;right:0;padding:10px;'
)

#'
#'
#+ include = FALSE
# Packages. ----
library("dplyr")
library("stringr")
library("ggplot2")
library("teutils")
library("ercotr")

# Constants. ----
theme_set(theme_minimal())

# Functions. ----


#'
#' # Introductiono/Purpose
#'
#' lorem ipsum...
#'
#' # Analysis
#'
#' lorem ipsum...
#'
#' ## Why...?
#'
# code

#'
#'
#+ results = "asis", fig.show = "asis"
# kable/viz


#'
#' ## How...?
#'

# code

#'
#'
#+ results = "asis", fig.show = "asis"
# kable/viz

#'
#' # Conclusion
#'
#' lorem ipsum...
#'
#+ include = FALSE

