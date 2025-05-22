#' Format standard weather parameters from a JSON representation of OpenMeteo data
#'
#' @param weather a JSON representation of OpenMeteo data, obtained from \code{\link{fetch_weather}}
#'
#' @returns a nested tibble with daily variables in columns, and tibbles of hourly variables in a list column (see details)
#'
#' @details
#' Daily variables have the following units:
#' - date: R formatted date
#' - weather_code: wmo code (TODO: lookup table)
#' - sunrise: R formatted date/time (UTC)
#' - sunset: R formatted date/time (UTC)
#'
#' Hourly variables have the following units:
#' - time: R formatted date/time (UTC)
#' - temperature_2m: °C
#' - relative_humidity_2m: %
#' - dew_point_2m: °C
#' - apparent_temperature: °C
#' - precipitation: mm
#' - snow_depth: m
#' - weather_code: wmo code
#' - surface_pressure: hPa
#' - wind_speed_10m: km/h
#' - wind_speed_100m: km/h
#' - wind_direction_10m: °
#' - wind_direction_100m: °
#' - wind_gusts_10m: km/h
#' - soil_temperature_0_to_7cm: °C
#' - soil_moisture_0_to_7cm: m³/m³
#' - sunshine_duration: s
#' - direct_radiation: W/m²
#' - et0_fao_evapotranspiration: mm
#' - terrestrial_radiation: W/m²
#'
#' @import dplyr
#' @import tibble
#' @import stringr
#' @importFrom rlang .data
#'
#' @export
format_weather <- function(weather){

  stopifnot(is.list(weather))
  stopifnot(length(weather[["latitude"]])==1L, length(weather[["longitude"]])==1L,
            length(weather[["timezone"]])==1L, weather[["timezone"]] %in% c("GMT","UTC"))
  stopifnot("daily" %in% names(weather), "hourly" %in% names(weather))

  # To extract units:
  # str_c("#' - ", bind_cols(names(weather[["hourly_units"]]), as.character(weather[["hourly_units"]])) |> apply(1, \(x) str_c(x, collapse=": ")), collapse="\n") |> cat()

  weather[["hourly"]] |>
    bind_cols() |>
    rename(date_time="time") |>
    mutate(date_time = as_datetime(.data$date_time, tz="UTC", format="%Y-%m-%dT%H:%M")) |>
    mutate(date = as_date(.data$date_time, tz="UTC")) |>
    select(.data$date, .data$date_time, everything()) ->
    hourly

  num_per_day <- hourly |> count(.data$date) |> pull(.data$n)
  stopifnot(num_per_day == 24L)

  weather[["daily"]] |>
    bind_cols() |>
    rename(date="time") |>
    mutate(date = as_date(.data$date, format="%Y-%m-%d")) |>
    mutate(across(c("sunrise","sunset"), \(x) as_datetime(x, tz="UTC", format="%Y-%m-%dT%H:%M"))) ->
    daily

  daily |>
    arrange(date) |>
    mutate(
      hourly = hourly |> arrange(.data$date) |> group_by(.data$date) |> group_split() |> lapply(\(x) x |> select(-.data$date))
    ) ->
    wthr

  ## TODO - lookup table for WMO codes:
  # Code	Description
  # 0	Clear sky
  # 1, 2, 3	Mainly clear, partly cloudy, and overcast
  # 45, 48	Fog and depositing rime fog
  # 51, 53, 55	Drizzle: Light, moderate, and dense intensity
  # 56, 57	Freezing Drizzle: Light and dense intensity
  # 61, 63, 65	Rain: Slight, moderate and heavy intensity
  # 66, 67	Freezing Rain: Light and heavy intensity
  # 71, 73, 75	Snow fall: Slight, moderate, and heavy intensity
  # 77	Snow grains
  # 80, 81, 82	Rain showers: Slight, moderate, and violent
  # 85, 86	Snow showers slight and heavy
  # 95 *	Thunderstorm: Slight or moderate
  # 96, 99 *	Thunderstorm with slight and heavy hail

  return(wthr)

}
