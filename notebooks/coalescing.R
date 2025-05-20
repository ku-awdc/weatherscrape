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

  ss <- try({
    grid <- to_scrape[i,]
    gid <- grid[["GridID"]]
    cat(gid, " (#", i, ") at ", as.character(Sys.time()), "\n", sep="")
    elevation <- grid[["MeanElevation"]]
    latlong <- grid |> pull(Centroid) |> st_transform(crs="WGS84") |> st_coordinates() |> as.numeric() |> rev()

    fn <- str_c(base_path, year, "/", gid, ".rqs")
    if(!file.exists(fn)){
      wthr <- fetch_weather(latlong[1], latlong[2], year, elevation, format=FALSE)
      qsave(wthr, fn)
      # 15 minute intervals are fine: results in 5760 API calls per day
      # i.e. it will take 1617/(4*24) = 17 days per year to scrape
      Sys.sleep(60*15)

      # Note: for rapid-fire scraping, we could do 15 second delays for 60 locations
      # (will take ~15 mins for 3600 API calls) - a full year will take 27 days
      # [this could be repeated up to twice per day, but don't do this]
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

  ## If we get an error, do a time out:
  if(inherits(ss, "try-error")){
    Sys.sleep(60*30)
  }

}

try({
  all_wthr <- bind_rows(all_wthr)
})

qsave(all_wthr, str_c(base_path, year, "/weather_", year, ".rqs"))
