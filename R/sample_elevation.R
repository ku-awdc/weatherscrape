#' Sample elevation for a specified polygon
#'
#' @param polygon an sf (or sfc) object representing a spatial polygon within which to sample
#' @param n_points the number of points to sample
#'
#' @details
#' One call to this function takes a single API call (I think)
#'
#' @import sf
#' @importFrom forcats fct
#' @importFrom tidyr expand_grid
#'
#' @export
sample_elevation <- function(polygon, n_points=100L){

  warning("Change grid argument to an sf or sfc object in sample_elevation")
  grid <- polygon

  stopifnot(is.data.frame(grid), inherits(grid, "sf"), nrow(grid)==1L)
  stopifnot(n_points >= 1L)

  pts <- st_sample(grid[["geometry"]], n_points, type="hexagonal", offset=grid[["Centroid"]] |> st_coordinates())

  # Either remove or add at random to make exactly 100:
  if(length(pts) > n_points){
    pts <- pts[sample.int(length(pts), n_points)]
  }else if(length(pts) < n_points){
    npts <- st_sample(grid[["geometry"]], n_points-length(pts), type="random")
    pts <- c(pts, npts)
  }
  stopifnot(length(pts)==n_points)

  longs <- st_coordinates(st_transform(pts, crs="WGS84"))[,1]
  lats <- st_coordinates(st_transform(pts, crs="WGS84"))[,2]

  ## Note: I assume this counts as 1 API request
  rqst <- str_c(
    "https://api.open-meteo.com/v1/elevation?latitude=",
    str_c(lats,collapse=","),
    "&longitude=",
    str_c(longs,collapse=",")
  )

  rr <- readLines(url(rqst), warn=FALSE)

  # ggplot() + geom_sf(data=grid) + geom_sf(data=pts)

  grid |>
    as_tibble() |>
    select(.data$GridID, .data$GridScale) |>
    expand_grid(
      tibble(
        Elevation = fromJSON(rr)[["elevation"]],
        geometry = pts
      )
    ) |>
    st_as_sf()

}
