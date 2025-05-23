library("weatherscrape")
library("tidyverse")
library("qs")

## Load 2024:
w2024 <- qread("/Users/qjv610/Documents/Resources/Datasets/OpenMeteo/y2024_l1617.rqs")

## Load 2023:
nf <- length(list.files("~/weather_scrape/y2023")) - 1L
locs <- weatherscrape::weather_locations |> arrange(Priority) |> slice(1:nf)
rv <- scrape_weather("2023", locations=locs, max_scrapes = 0L)
w2023 <- qread(rv$file)
unlink(rv$file)

bind_rows(
  w2023,
  w2024
) |>
  left_join(weatherscrape::weather_locations, by="ID") |>
  rowwise() |>
  mutate(hourly = hourly |> summarise(mean_temp = mean(temperature_2m))) |>
  ungroup() |>
  unnest(hourly) ->
  all_weather

all_weather |>
  filter(GridScale == "100x100km") |>
  ggplot(aes(x=date, y=mean_temp, col=Latitude, group=ID)) +
  geom_line()

all_weather |>
  filter(GridScale == "100x100km") |>
  ggplot(aes(x=date, y=mean_temp, col=Longitude, group=ID)) +
  geom_line()

all_weather |>
  filter(GridScale == "100x100km") |>
  ggplot(aes(x=date, y=mean_temp, col=MeanElevation, group=ID)) +
  geom_line()

all_weather |>
  filter(date>"2024-01-01", date<"2024-01-10") |>
  arrange(desc(mean_temp)) |>
  slice(1:10)

## Something was wrong with E41N36 for 100x100km!!!
all_weather |>
  filter(GridScale == "100x100km") |>
  filter(ID=="E41N36") |>
  ggplot(aes(x=date, y=mean_temp, col=ID=="E41N36", group=ID)) +
  geom_line()
## Re-scrape and re-save:
if(FALSE){
  locs <- locs |> filter(ID=="E41N36")
  scrape_weather("2024", locations=locs, interval="15f")
  redo <- qread("~/weather_scrape/y2024_l1.rqs")
  w2024 |>
    filter(ID!="E41N36") |>
    bind_rows(
      redo
    ) ->
    w2024
  stopifnot(weather_locations[["ID"]] %in% w2024[["ID"]], nrow(w2024)==366*nrow(weather_locations))
  qsave(w2024, "/Users/qjv610/Documents/Resources/Datasets/OpenMeteo/y2024_l1617.rqs", preset="archive")
}



all_weather |>
  filter(GridScale == "10x10km") |>
  ggplot(aes(x=date, y=mean_temp, col=Latitude, group=ID)) +
  geom_line()

all_weather |>
  group_by(Year = strftime(date, "%Y"), ID, Latitude, Longitude, GridScale, MeanElevation) |>
  summarise(mean_temp = mean(mean_temp), .groups="drop") |>
  ggplot(aes(x=Latitude, y=mean_temp, col=MeanElevation)) +
  geom_point() +
  facet_wrap(~Year)

all_weather |>
  group_by(Year = strftime(date, "%Y"), ID, Latitude, Longitude, GridScale, MeanElevation) |>
  summarise(mean_temp = mean(mean_temp), .groups="drop") |>
  ggplot(aes(x=Longitude, y=Latitude, col=mean_temp)) +
  geom_point() +
  facet_wrap(~Year)

all_weather |>
  group_by(Year = strftime(date, "%Y"), ID, Latitude, Longitude, GridScale, MeanElevation) |>
  summarise(mean_temp = mean(mean_temp), .groups="drop") |>
  pivot_wider(names_from="Year", values_from="mean_temp") |>
  ggplot(aes(x=`2023`, y=`2024`, col=Latitude)) +
  geom_point()
