library("tidyverse")
library("sf")
library("units")
library("qs")
library("weatherscrape")

(load("~/Documents/GitHub/weatherscrape/notebooks/grids_with_el.rda"))

base_path <- "~/Documents/Resources/Datasets/OpenMeteo/"
year <- "2024"
to_scrape <- grids

all_wthr <- vector("list", length=nrow(grids))

for(i in seq_len(nrow(to_scrape))){

  try({
    grid <- to_scrape[i,]
    gid <- grid[["GridID"]]
    cat(gid, " (#", i, ") at ", as.character(Sys.time()), "\n", sep="")
    elevation <- grid[["MeanElevation"]]
    latlong <- grid |> pull(Centroid) |> st_transform(crs="WGS84") |> st_coordinates() |> as.numeric() |> rev()

    fn <- str_c(base_path, year, "/", gid, ".rqs")
    if(!file.exists(fn)){
      wthr <- fetch_weather(latlong[1], latlong[2], year, elevation, format=FALSE)
      qsave(wthr, fn)
      Sys.sleep(60*30)
    }

    stopifnot(file.exists(fn))
    format_weather(qread(fn)) |>
      bind_cols(
        grid |> as_tibble() |> select(GridID)
      ) |>
      select(GridID, everything()) ->
      wthr

    all_wthr[[i]] <- wthr

  })

}

try({
  all_wthr <- bind_rows(all_wthr)
})

qsave(all_wthr, str_c(base_path, year, "/weather_", year, ".rqs"))
