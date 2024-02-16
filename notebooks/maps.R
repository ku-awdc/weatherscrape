library("tidyverse")
library("qs")
library("weatherscrape")

## Maps
plotonmap <- function(locations){
  mp <- maps::map("world", plot=FALSE, fill=TRUE) |> sf::st_as_sf()
  loc <- locations |> sf::st_as_sf(coords=c("Longitude","Latitude"), crs="WGS84")
  ggplot() +
    geom_sf(data=mp) +
    geom_sf(data=loc, col="red", size=0.5)
}

locs <- qread(file.path(Sys.getenv("WEATHER_FOLDER"), "metadata.rqs"))
plotonmap(locs)
nrow(locs)


## Weird fix needed ??
is_anholt <- which(locs$Location=="S_8165")
locs$Latitude[is_anholt] <- 56.65
locs$Longitude[is_anholt] <- 11.50

weekno <- 13

tibble(Date = seq.Date(as.Date("2023-01-01"),as.Date("2023-12-31"),by=1L)) |>
  mutate(Week = strftime(Date, "%V") |> as.numeric()) |>
  filter(Week==weekno) |>
  arrange(Date) |>
  slice(1L) |>
  pull(Date) ->
  start_date

stopifnot(strftime(as.Date(start_date), "%w") == "1")
fn <- file.path(Sys.getenv("WEATHER_FOLDER"), "historical", str_c(strftime(as.Date(start_date), "%Y_W%V"), ".rqs"))
stopifnot(!file.exists(fn))
fn
start_date

res <- scrape_historical(locs, start_date=start_date, n_days=7L, time_out=0.1)
qsave(res, fn)



failed <- sapply(res, function(x) length(x$list))!=(7*24)
sum(failed)
for(f in which(failed)){
  ll <- as.character(res[[f]]$location)
  cat(ll, " ... ")
  tries <- 0L
  repeat{
    Sys.sleep(1L)
    nr <- scrape_historical(locs |> filter(Location==ll), start_date=start_date, n_days=7L, time_out=0.1)
    if(length(nr[[1]]$list)==((7*24))) break
    tries <- tries + 1L
    if(tries > 5L){
      tries <- tries + 1L
      cat("failed!")
      break
    }
  }
  cat("\n")
  res[[f]] <- nr[[1]]
}


if(FALSE){
  fn <- file.path(Sys.getenv("WEATHER_FOLDER"), "historical", "2023_W02.rqs")
  dat <- qread(fn)
  sapply(dat, function(x) length(x$list)) |> table()
}

