#' teproj
#'
#' This is Tony's package for projects.
#'
#' @name teproj
#' @docType package
#' @import rio
NULL

utils::globalVariables(c("."))
# sinew::makeImport("R", format = "description", desc_loc = ".")
# rstudioapi::getActiveDocumentContext()$path
# kimisc::thisfile()

# Notepad Plus Plus find and replace all
# Find What: (^#'.*)`(.*)`
# Replace with: \1\\code\{\2\}
# Filters: *
