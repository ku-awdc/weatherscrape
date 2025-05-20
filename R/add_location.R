#' Title
#'
# ' @importFrom gtools baseOf
#'
#' @export
add_location <- function(locations, historical=FALSE, current=FALSE, forecast=FALSE){

  stopifnot(test_weather_folder())

  stopifnot(c("Location","Latitude","Longitude") %in% names(locations))
  if(!"Notes" %in% names(locations)) locations[["Notes"]] <- character(0L)

  folder <- ws_env[["folder"]]
  locdata <- qread(file.path(folder, "metadata.rqs"))
  stopifnot(all(table(locations[["Location"]])==1L))
  if(any(locations[["Location"]] %in% locdata[["Location"]]))
    browser()

  locations[["Index"]] <- seq(nrow(locdata)+1L, nrow(locdata)+nrow(locations), by=1L) |> base62string()
  locations[["Added"]] <- Sys.Date()
  locations[["Historical"]] <- historical
  locations[["Current"]] <- current
  locations[["Forecast"]] <- forecast

  locdata <- bind_rows(locdata, locations)
  qsave(locdata, file.path(folder, "metadata.rqs"))

  stopifnot(all(table(locdata[["Location"]])==1L))

  ## TODO: checks for Lat/Long and show map or not
  invisible(locdata)
}


base62string <- function(x, length = 4){

  stopifnot(is.numeric(x), x%%1 == 0L, x<(62^length))
  nums <- baseOf(x, base=62L, len=length)
  nums[nums!=0] <- c(1:9,letters,LETTERS)[nums]
  as.character(apply(nums,1,paste,collapse=""))

}
