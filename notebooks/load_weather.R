library("qs")
library("tidyverse")

(load("~/Documents/GitHub/weatherscrape/notebooks/grids.rda"))

base_path <- "~/Documents/Resources/Datasets/OpenMeteo/"
year <- "2024"

list.files(file.path(base_path, year))
gid <- "E42N35"

grids |>
  filter(GridID == gid)

elev <- qread(file.path(base_path, year, str_c(gid, ".rqs")))
weather <- qread(file.path(base_path, year, str_c(gid, ".rqs")))
