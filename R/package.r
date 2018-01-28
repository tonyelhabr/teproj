#' teproj
#'
#' This is Tony's package for projects.
#'
#' @name teproj
#' @docType package
#' @import readr
#' @importFrom utils globalVariables
NULL

utils::globalVariables(c("."))

# NOTE: I think the rio::.import and rio::.export functions
# in the parse_proj_io.R file gives the below command an error, so run the lines after it.
# # sinew::makeImport("R", format = "description", desc_loc = ".")
# r_files <- grep("parse", list.files("R", full.names = TRUE), value = TRUE, invert = TRUE)
# sinew::makeImport(r_files, format = "oxygen", desc_loc = ".")
# sinew::makeImport(r_files, format = "description", desc_loc = ".")

# rstudioapi::getActiveDocumentContext()$path
# kimisc::thisfile()

# Notepad Plus Plus find and replace all
# Find What: (^#'.*)`(.*)`
# Replace with: \1\\code\{\2\}
# Filters: *