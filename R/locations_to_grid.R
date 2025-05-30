#' Convert a data frame of locations (with Latitude and Longitude) into grid IDs
#'
#' @param locations a data frame or tibble including columns Latitude and Longitude
#' @param grid_size either 10km or 100km
#'
#' @export
locations_to_grid <- function(locations, grid_size = c("10km", "100km")){

  grid_size <- matchArg(grid_size) |> str_sub(1, -3) |> as.integer()

  stopifnot(is.data.frame(locations), c("Latitude","Longitude") %in% names(locations), !c("Coords","East","North","GridID") %in% names(locations))

  locations |>
    as_tibble() |>
    st_as_sf(coords = c("Longitude", "Latitude")) |>
    st_set_crs("WGS84") |>
    st_transform("EPSG:3035") |>
    mutate(Coords = st_coordinates(.data$geometry)) |>
    mutate(East = floor(.data$Coords[,1]/(grid_size*1000)), North = floor(.data$Coords[,2]/(grid_size*1000))) |>
    mutate(GridID = str_c("E", .data$East, "N", .data$North)) ->
    locations

  stopifnot(locations$East > 0, locations$North > 0, str_length(locations$GridID) == 2+str_length(1000/grid_size)*2)

  locations |>
    as_tibble() |>
    select(-.data$geometry, -.data$Coords, -.data$East, -.data$North)
}
