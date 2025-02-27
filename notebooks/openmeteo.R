library("openmeteo")

cat(weather_variables()$hourly_history_vars, sep='", "')
hourly <- c("temperature_2m", "relative_humidity_2m", "dew_point_2m", "apparent_temperature", "pressure_msl",
            "wind_speed_10m", "wind_speed_100m", "wind_direction_10m", "wind_direction_100m", "wind_gusts_10m",
            "shortwave_radiation", "direct_radiation", "direct_normal_irradiance", "diffuse_radiation",
            "vapour_pressure_deficit", "et0_fao_evapotranspiration", "precipitation", "rain", "snowfall",
            "soil_temperature_0_to_7cm", "soil_temperature_7_to_28cm",
            "soil_moisture_0_to_7cm", "soil_moisture_7_to_28cm")

cat(weather_variables()$daily_history_vars, sep='", "')
daily <- c("sunrise", "sunset", "sunshine_duration",
           "precipitation_hours", "precipitation_sum",
           "apparent_temperature_mean", "temperature_2m_mean",
           "wind_direction_10m_dominant", "weather_code")


## For a period of 3 months this is about 20.6 calls, so assume 25
## There is a max of 600 per minute, so a fetch with 5 second delay is fine
## Up to a max of 5000 calls per hour (and don't use 10000 per day) so max 200 fetches per day
## 200 fetches is 50 place-years of data per day ... should take around 30 mins to scrape




location <- c(55.7, 12.6)

# 4.5 calls per month and location (regardless of number of months or locations) - so round to 5
# Or 3.9 for daily plus 0.9 for hourly
hourly <- c("temperature_2m", "relative_humidity_2m", "dew_point_2m", "apparent_temperature",
            "wind_speed_10m", "wind_speed_100m", "wind_direction_10m", "wind_direction_100m", "wind_gusts_10m",
            "direct_radiation", "et0_fao_evapotranspiration", "precipitation",
            "soil_temperature_0_to_7cm", "soil_moisture_0_to_7cm", "surface_pressure", "sunshine_duration",
            "terrestrial_radiation", "snow_depth")
daily <- c("sunrise", "sunset", "wind_direction_10m_dominant", "weather_code")


hh <- weather_history(round(location,3), "2024-01-01", "2024-12-31", hourly=hourly, daily=daily,
                response_units=list(temperature_unit = "celsius", windspeed_unit = "kmh", precipitation_unit = "mm"),
                timezone="UTC")

# Note: the hourly data for midnight comes with an NA time, but it is midnight except for the daily data!

hh |>
  mutate(latitude = location[1], longitude = location[2]) |>
  replace_na(list(time="00:00:00")) |>
  arrange(date, time) |>
  mutate(date_time = as_datetime(str_c(date, "T", time, "z"))) |>
  select(latitude, longitude, date_time, everything()) |>
  select(!starts_with("daily"), -date, -time) |>
  rename_with(function(x) str_replace(x, "hourly_", "")) |>
  identity() ->
  hourly_data

## Then separate starts_with("daily") from starts_with("hourly") and discard is.na(time) or !is.na(time) accordingly
