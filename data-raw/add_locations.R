library("tidyverse")
library("sf")

(load("notebooks/grids_with_el.rda"))


grids |>
  mutate(DistDK = st_distance(
    Centroid,
    countries |> filter(NUTS=="DK") |> pull(geometry) |> st_centroid(of_largest_polygon = FALSE)
  )[,1L] |> as.numeric() |> {\(x) x/1e3}()) |>
  arrange(GridScale, DistDK) |>
  mutate(Priority = row_number()) |>
  mutate(LongLat = st_coordinates(Centroid |> st_transform("WGS84"))) |>
  mutate(Latitude = LongLat[,2], Longitude = LongLat[,1]) |>
  as_tibble() |>
  select(Priority, ID=GridID, GridScale, Latitude, Longitude, MeanElevation) ->
  weather_locations

ggplot() +
  geom_sf(data=countries) +
  geom_sf(data=weather_locations |> st_as_sf(coords = c("Longitude", "Latitude"), crs="WGS84"), mapping=aes(col=MeanElevation))

usethis::use_data(weather_locations, overwrite = TRUE)
