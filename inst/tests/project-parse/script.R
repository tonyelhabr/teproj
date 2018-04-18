
library("datasets")
library("bindrcpp")
library("dplyr")

olddir <- getwd()
newdir <- "tests/testthat/fake_project"
setwd(newdir)

path <- export_ext_csv(iris)
iris <- import_ext_csv(iris)
unlink(path)

# Different extension and different data set.
path <- export_ext_rds(mtcars)
mtcars <- import_ext_rds(mtcars)
unlink(path)

# Different save/read pattern and different data set.
path <- export_ext(crimtab, ext = "rda")
crimtab <- import_ext(crimtab, ext = "rda")
unlink(path)

path <- export_ext(faithful, ext = "xlsx")
diamonds <- import_ext(path = path, ext = "xlsx")
unlink(path)

# path <- paste0(dir, "cars.RData")
export_ext(cars, path = paste0(dir, "cars.RData"), ext = "RData")
cars <- import_ext(path = paste0(dir, "cars.RData"), ext = "RData")

path <- airquality %>% export_ext_feather()
unlink(path)

path <-
  Chickweight %>%
  export_ext_feather()
unlink(path)

path <- rock %>%
  export_ext(ext = "feather")
unlink(path)

path <- quakes %>% export_ext(ext = "feather")
unlink(path)

library("readr")
# Get the path.
path <- export_ext_csv(iris)
iris %>% readr::write_csv(path = path)
iris %>% write_csv(path = path)
iris <- readr::read_csv(file = path)
iris <- read_csv(file = path)
iris <- read_csv(path)
unlink(path)

setwd(olddir)
rm(list = ls())
