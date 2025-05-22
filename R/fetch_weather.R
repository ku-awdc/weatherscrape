#' Fetch standard weather parameters from OpenMeteo for a given latitude/longitude.
#'
#' @details
#' Fetching data for a 12-month period counts as ~60 (2024=57.4) API calls, so you must not
#' exceed 10 locations per minute and/or 80 per hour and/or 160 per day (for a
#' full year at a time) - see https://open-meteo.com/en/terms.
#' Modifying the time period has a linear effect on the number of API calls.
#'
#' @param latitude a scalar latitude (WGS84)
#' @param longitude a scalar longitude (WGS84)
#' @param year a single year to fetch (ignored if start_date/end_date are changed)
#' @param elevation the elevation to use for scraping (optional; if missing the elevation of the point location will be used)
#' @param start_date start date for the relevant period (inclusive)
#' @param end_date end date for the relevant period (inclusive)
#' @param format should the weather data be formatted using \code{\link{format_weather}} before returning?
#'
#' @returns a list of data frames giving (1) daily variables, (2) hourly variables (in UTC) for the provided latitude/longitude/elevation
#'
#' @importFrom checkmate qassert assert_number assert_date assert_numeric assert_vector
#' @importFrom stringr str_c
#' @importFrom lubridate as_date as_datetime
#' @importFrom jsonlite fromJSON
#'
#' @export
fetch_weather <- function(latitude, longitude, year, elevation=NA_real_, start_date = as_date(str_c(year, "-01-01")), end_date = as_date(str_c(year, "12-31")), format=TRUE){

  assert_number(latitude, lower=-90, upper=90)
  assert_number(longitude, lower=-180, upper=180)
  assert_number(elevation, na.ok=TRUE, lower=-500, upper=10000) # Note: lowest possible place is below sea level!
  assert_date(start_date, any.missing=FALSE, len=1L, lower=as_date("1900-01-01"))
  assert_date(end_date, any.missing=FALSE, len=1L, lower=start_date, upper=(Sys.Date() - 7L))

  rqst <- str_c(
    "https://archive-api.open-meteo.com/v1/archive?latitude=",
    latitude,
    "&longitude=",
    longitude,
    "&start_date=",
    start_date,
    "&end_date=",
    end_date,
    "&daily=weather_code,sunrise,sunset&hourly=temperature_2m,relative_humidity_2m,dew_point_2m,apparent_temperature,precipitation,snow_depth,weather_code,surface_pressure,wind_speed_10m,wind_speed_100m,wind_direction_10m,wind_direction_100m,wind_gusts_10m,soil_temperature_0_to_7cm,soil_moisture_0_to_7cm,sunshine_duration,direct_radiation,et0_fao_evapotranspiration,terrestrial_radiation&timezone=UTC")

  if(!is.na(elevation)){
    rqst <- str_c(rqst, "&elevation=", elevation)
  }

  rr <- readLines(url(rqst), warn=FALSE)
  wthr <- fromJSON(rr)

  if(format){
    wthr <- format_weather(wthr)
    stopifnot(nrow(wthr) == ((end_date - start_date) + 1L))
  }

  return(wthr)

}
