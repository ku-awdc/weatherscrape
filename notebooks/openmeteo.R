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


location <- c(55.7, 12.6)

## For a period of 3 months this is about 20.6 calls, so assume 25
## There is a max of 600 per minute, so a fetch with 5 second delay is fine
## Up to a max of 5000 calls per hour (and don't use 10000 per day) so max 200 fetches per day
## 200 fetches is 50 place-years of data per day ... should take around 30 mins to scrape

hh <- weather_history(location, "2024-01-01", "2024-01-02", hourly=hourly, daily=daily,
                response_units=list(temperature_unit = "celsius", windspeed_unit = "kmh", precipitation_unit = "mm"),
                timezone="UTC")

hh

## Then separate starts_with("daily") from starts_with("hourly") and discard is.na(time) or !is.na(time) accordingly
