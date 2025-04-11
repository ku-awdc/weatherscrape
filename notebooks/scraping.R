library("tidyverse")
library("sf")
library("units")
library("jsonlite")
library("qs")

(load("~/Documents/GitHub/weatherscrape/notebooks/grids.rda"))

## Find elevation for 100 coordinates within the provided box:
sample_elevation <- function(grid, n_points=100L){

  stopifnot(is.data.frame(grid), inherits(grid, "sf"), nrow(grid)==1L)
  stopifnot(n_points >= 1L)

  pts <- st_sample(grid[["geometry"]], n_points, type="hexagonal", offset=grid[["Centroid"]] |> st_coordinates())

  # Either remove or add at random to make exactly 100:
  if(length(pts) > n_points){
    pts <- pts[sample.int(length(pts), n_points)]
  }else if(length(pts) < n_points){
    npts <- st_sample(grid[["geometry"]], n_points-length(pts), type="random")
    pts <- c(pts, npts)
  }
  stopifnot(length(pts)==n_points)

  longs <- st_coordinates(st_transform(pts, crs="WGS84"))[,1]
  lats <- st_coordinates(st_transform(pts, crs="WGS84"))[,2]

  ## Note: I assume this counts as 1 API request
  rqst <- str_c(
    "https://api.open-meteo.com/v1/elevation?latitude=",
    str_c(lats,collapse=","),
    "&longitude=",
    str_c(longs,collapse=",")
  )

  rr <- readLines(url(rqst), warn=FALSE)

  # ggplot() + geom_sf(data=grid) + geom_sf(data=pts)

  grid |>
    as_tibble() |>
    select(GridID, GridScale) |>
    expand_grid(
      tibble(
        Elevation = fromJSON(rr)[["elevation"]],
        geometry = pts
      )
    ) |>
    st_as_sf()

}

if(FALSE){
mont_blanc <- st_point(c(6.8652, 45.8326)) |> st_sfc(crs=st_crs("WGS84")) |> st_transform(crs=st_crs(grids))
grids |>
  filter(st_intersects(geometry, mont_blanc, sparse=FALSE)[,1]) ->
  grid

grids |>
  filter(Area < max(Area)) |>
  slice_sample(n=1) ->
  grid

pts <- sample_elevation(grid)
ggplot() + geom_sf(data=grid) + geom_sf(data=pts, mapping=aes(col=Elevation))
}

## API usage
# Assume elevation is 1 call, so total of 100 calls per location to get 1 year of data
# Max 600 calls per min means 10 second delay between calls
# Max 5000 calls per hour or 10000 per day, but stick to 5000 per day so I don't have to worry about it
# So we can start a script that at e.g. midday scrapes 1 year of data for 50 locations
# Or, even easier, scrape 1 location per 30 mins (48 per day)

# Ordering of locations by country, with priority for DK/SE/NO/DE/UK/NL/BE/PL
countries |>
  filter(NUTS %in% c("DK","SE","NO","DE","UK","NL","BE","PL")) |>
  st_union() ->
  priority

grids |>
  mutate(Priority = st_intersects(geometry, priority, sparse=FALSE)[,1]) |>
  mutate(DistDK = st_distance(geometry, countries |> filter(NUTS=="DK") |> st_union())[,1]) |>
  filter(GridScale=="100x100km") |>
  arrange(desc(Priority), DistDK) ->
  to_scrape

# 4.5 calls per month and location (regardless of number of months or locations) - so round to 5
hourly <- c("temperature_2m", "relative_humidity_2m", "dew_point_2m", "apparent_temperature",
            "wind_speed_10m", "wind_speed_100m", "wind_direction_10m", "wind_direction_100m", "wind_gusts_10m",
            "direct_radiation", "et0_fao_evapotranspiration", "precipitation",
            "soil_temperature_0_to_7cm", "soil_moisture_0_to_7cm", "surface_pressure", "sunshine_duration",
            "terrestrial_radiation", "snow_depth")
daily <- c("sunrise", "sunset", "weather_code")

base_path <- "~/Documents/Resources/Datasets/OpenMeteo/"
year <- "2024"

if(!dir.exists(file.path(base_path, year))){
  dir.create(file.path(base_path, year))
}
date_range <- str_c(year, c("-01-01","-12-31"))

for(i in seq_len(nrow(to_scrape))){

  try({
    grid <- to_scrape[i,]
    gid <- grid[["GridID"]]
    cat(gid, " (#", i, ") at ", as.character(Sys.time()), "\n", sep="")

    fn <- str_c(base_path, "elevations/", gid, ".rqs")
    if(!file.exists(fn)){
      elevation <- sample_elevation(grid)
      qsave(elevation, fn)
    }
    stopifnot(file.exists(fn))
    elevation <- qread(fn)

    melev <- elevation |> summarise(Elevation = mean(Elevation)) |> pull(Elevation)
    stopifnot(!is.na(melev))

    latlong <- grid |> pull(Centroid) |> st_transform(crs="WGS84") |> st_coordinates() |> as.numeric() |> rev()

    rqst <- str_c(
      "https://archive-api.open-meteo.com/v1/archive?latitude=",
      latlong[1],
      "&longitude=",
      latlong[2],
      "&start_date=",
      date_range[1],
      "&end_date=",
      date_range[2],
      "&daily=weather_code,sunrise,sunset&hourly=temperature_2m,relative_humidity_2m,dew_point_2m,apparent_temperature,precipitation,snow_depth,weather_code,surface_pressure,wind_speed_10m,wind_speed_100m,wind_direction_10m,wind_direction_100m,wind_gusts_10m,soil_temperature_0_to_7cm,soil_moisture_0_to_7cm,sunshine_duration,direct_radiation,et0_fao_evapotranspiration,terrestrial_radiation&timezone=UTC&elevation=",
      melev
    )

    fn <- str_c(base_path, year, "/", gid, ".rqs")
    if(!file.exists(fn)){
      rr <- readLines(url(rqst), warn=FALSE)
      wthr <- fromJSON(rr)
      qsave(wthr, fn)
      Sys.sleep(60*30)
    }else{
      # Sleep for a min due to the possible extraction of elevation
      Sys.sleep(60)
    }

  })

}

