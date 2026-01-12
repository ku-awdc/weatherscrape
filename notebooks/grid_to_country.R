library("tidyverse")
library("sf")

(load("notebooks/grids_with_el.rda"))

countries |>
  rowwise() |>
  group_split() |>
  map(\(x){
    grids |>
      filter(GridScale=="100x100km") |>
      filter(st_intersects(geometry, x$geometry, sparse=FALSE)[,1]) |>
      mutate(geometry = st_intersection(geometry, x$geometry)) |>
      mutate(Overlap = st_area(geometry)) |>
      as_tibble() |>
      mutate(Weight = as.numeric(Overlap) / as.numeric(st_area(x$geometry))) |>
      bind_cols(
        x |> as_tibble() |> select(NUTS, Label)
      ) |>
      select(NUTS, Label, GridID, GridScale, Weight, geometry)
  }, .progress=TRUE) |>
  bind_rows() |>
  identity() ->
  overlaps

overlaps |>
  group_by(NUTS) |>
  summarise(Total = sum(Weight)) |>
  pull(Total) |>
  print(digits=12)

st_intersects(grids[1:5,], countries[1:5,])
grids
countries

saveRDS(overlaps, "notebooks/overlaps.rds")

overlaps |>
  filter(NUTS=="DK", Weight > 0.01) ->
  dks


## Extract Danish data for 2007-2009:

library("qs")

2007:2009 |>
  as.list() |>
  map(\(yy){
    dd <- qread(str_c("/Users/matthewdenwood/Documents/Resources/Datasets/OpenMeteo/y",yy,"_l1617.rqs"))
    dd |>
      filter(ID %in% dks$GridID) |>
      select(ID, hourly) |>
      unnest(hourly)
  }, .progress=TRUE) |>
  bind_rows() ->
  denmark

saveRDS(denmark, "denmark_2007..09.rds")
