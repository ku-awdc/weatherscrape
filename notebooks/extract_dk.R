library("tidyverse")
library("sf")
library("qs")
library("weatherscrape")

path <- "~/Documents/Resources/Datasets/OpenMeteo/"

## Data for Amalie:
tribble(
  ~Location, ~Lat, ~Long,
  "Saltbækvig", 55.715510, 11.246328,
  "Lille Vildmose", 56.846251, 10.214798,
) |>
  st_as_sf(coords = c("Long","Lat")) |>
  identity() ->
  coords
coords

grid_size <- 10

coords |>
  st_set_crs("WGS84") |>
  st_transform("EPSG:3035") |>
  mutate(Coords = st_coordinates(.data$geometry)) |>
  mutate(East = floor(.data$Coords[,1]/(grid_size*1000)), North = floor(.data$Coords[,2]/(grid_size*1000))) |>
  mutate(GridID = str_c("E", .data$East, "N", .data$North)) ->
  locations

2020:2024 |>
  map(\(y){
    locations |> as_tibble() |> select(ID=GridID, Location) |>
      left_join(
        qread(file.path(path, str_c("y",y,"_l1617.rqs"))),
        join_by(ID)
      )
  }, .progress=TRUE) |>
  bind_rows() ->
  data

saveRDS(data, "raw_data.rds")

data |>
  mutate(summarised = map(hourly, \(x){
    x |>
      summarise(across(c(
        temperature_2m, apparent_temperature,
        relative_humidity_2m, dew_point_2m,
        soil_moisture_0_to_7cm, soil_temperature_0_to_7cm),
        mean)) |>
      mutate(precipitation = sum(x$precipitation))
  }, .progress=TRUE)) |>
  select(-ID, -weather_code, -sunrise, -sunset, -hourly) |>
  unnest(cols=c(summarised)) ->
  summarised

summarised |>
  ggplot(aes(x=date, y=apparent_temperature, col=Location)) +
  geom_line()

summarised |>
  ggplot(aes(x=date, y=temperature_2m, col=Location)) +
  geom_line()

summarised |>
  filter(year(date)==2020) |>
  ggplot(aes(x=date, y=precipitation, col=Location, fill=Location)) +
  geom_col()

saveRDS(summarised, "summarised_data.rds")

writexl::write_xlsx(summarised, "summarised_data.xlsx")

weather <- readRDS("summarised_data.rds")
