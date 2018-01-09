
library("datasets")
library("bindrcpp")
library("dplyr")

olddir <- getwd()
newdir <- "tests/testthat/fake_project"
setwd(newdir)

filepath <- export_ext_csv(iris)
iris <- import_ext_csv(iris)
unlink(filepath)

# Different extension and different data set.
filepath <- export_ext_rds(mtcars)
mtcars <- import_ext_rds(mtcars)
unlink(filepath)

# Different save/read pattern and different data set.
filepath <- export_ext(crimtab, ext = "rda")
crimtab <- import_ext(crimtab, ext = "rda")
unlink(filepath)

filepath <- export_ext(faithful, ext = "xlsx")
diamonds <- import_ext(filepath = filepath, ext = "xlsx")
unlink(filepath)

# filepath <- paste0(dir, "cars.RData")
export_ext(cars, filepath = paste0(dir, "cars.RData"), ext = "RData")
cars <- import_ext(filepath = paste0(dir, "cars.RData"), ext = "RData")

filepath <- airquality %>% export_ext_feather()
unlink(filepath)

filepath <-
  Chickweight %>%
  export_ext_feather()
unlink(filepath)

filepath <- rock %>%
  export_ext(ext = "feather")
unlink(filepath)

filepath <- quakes %>% export_ext(ext = "feather")
unlink(filepath)

library("readr")
# Get the filepath.
filepath <- export_ext_csv(iris)
iris %>% readr::write_csv(path = filepath)
iris %>% write_csv(path = filepath)
iris <- readr::read_csv(file = filepath)
iris <- read_csv(file = filepath)
iris <- read_csv(filepath)
unlink(filepath)

setwd(olddir)
rm(list = ls())
