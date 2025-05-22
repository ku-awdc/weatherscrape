set_weather_folder <- function(folder){
  stopifnot(dir.exists(folder))

  if(!dir.exists(file.path(folder, "historical"))) stopifnot("Failed to create historical weather folder" = dir.create(file.path(folder, "historical")))
  if(!dir.exists(file.path(folder, "current"))) stopifnot("Failed to create current weather folder" = dir.create(file.path(folder, "current")))
  if(!dir.exists(file.path(folder, "forecast"))) stopifnot("Failed to create forecast weather folder" = dir.create(file.path(folder, "forecast")))

  fn <- file.path(folder, "metadata.rqs")
  if(!file.exists(fn)){
    dt <- tibble(
      Index = character(0L),
      Added = as.Date(character(0L)),
      Location = character(0L),
      Latitude = numeric(0L),
      Longitude = numeric(0L),
      Historical = logical(0L),
      Current = logical(0L),
      Forecast = logical(0L),
      Notes = character(0L)
    )
    qsave(dt, file.path(folder, "metadata.rqs"))
  }

  locdata <- qread(file.path(folder, "metadata.rqs"))
  stopifnot(c("Index","Added","Location","Latitude","Longitude","Historical","Current","Forecast","Notes") %in% names(locdata))

  ws_env[["folder"]] <- folder

  invisible(locdata)
}

test_weather_folder <- function(){
  sf <- Sys.getenv("WEATHER_FOLDER")
  if(sf!="" && dir.exists(sf)){
    set_weather_folder(sf)
  }

  file.exists(file.path(ws_env[["folder"]], "metadata.rqs"))
}

ws_env <- new.env()
ws_env[["folder"]] <- ""
