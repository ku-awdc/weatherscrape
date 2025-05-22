scrape_historical <- function(locations, start_date, n_days=7L, time_out=0.25){

  stopifnot(test_weather_folder())
  credentials <- load_credentials()

  if(is.character(start_date)) start_date <- as.Date(start_date)
  stopifnot(inherits(start_date, "Date"), start_date < Sys.Date())
  stopifnot(n_days > 0L, n_days <= 7L)
  ct <- (n_days*24L)
  start <- floor(as.numeric(as.POSIXct(str_c(strftime(start_date, "%Y-%m-%d"), "T00:00:00Z"), tz="UTC")))

  ff <- function(ll){

    Sys.sleep(time_out)
    ss <- try({
      lat <- ll$Latitude  # |> round(digits=3L) |> as.character()
      lon <- ll$Longitude  #|> round(digits=3L) |> as.character()

      wthr <- fromJSON(str_c("https://history.openweathermap.org/data/2.5/history/city?lat=",lat,"&lon=",lon,"&type=hour&start=", start, "&cnt=",ct,"&appid=", credentials))
      if(length(wthr$list)!=ct) warning("Abnormal length return value from OpenWeather")

      wthr$status <- "success"
      wthr$location <- ll$Location

      return(wthr)
    })

    return(list(location=ll$Location, status=ss))
  }

  if(nrow(locations)==1L){
    applyfun <- lapply
  }else{
    applyfun <- pblapply
  }

  stopifnot(all(table(locations$Location)==1L))
  locations |>
    ## Preserve ordering:
    mutate(Location = factor(.data$Location, levels=.data$Location)) |>
    group_split(.data$Location) |>
    applyfun(ff) ->
    res

  res
}
