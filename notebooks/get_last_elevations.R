library("tidyverse")
library("sf")
library("units")
library("jsonlite")
library("qs")

(load("~/Documents/GitHub/weatherscrape/notebooks/grids.rda"))

## Find elevation for 100 coordinates within the provided box:
sample_elevation <- function(grid, n_points=100L){

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
    select(GridID, GridScale) |>
    expand_grid(
      tibble(
        Elevation = fromJSON(rr)[["elevation"]],
        geometry = pts
      )
    ) |>
    st_as_sf()

}

base_path <- "~/Documents/Resources/Datasets/OpenMeteo/"
year <- "2024"

if(!dir.exists(file.path(base_path, year))){
  dir.create(file.path(base_path, year))
}

grids$mean_elevation <- NA_real_
all_els <- vector("list", length=nrow(grids))
for(i in seq_len(nrow(grids))){
  try({
    grid <- grids[i,]
    gid <- grid[["GridID"]]
    cat(gid, " (#", i, ") at ", as.character(Sys.time()), "\n", sep="")

    fn <- str_c(base_path, "elevations/", gid, ".rqs")
    if(!file.exists(fn)){
      elevation <- sample_elevation(grid)
      qsave(elevation, fn)
      # Sleep for 1 min:
      Sys.sleep(60)
    }
    stopifnot(file.exists(fn))
    elevation <- qread(fn)

    grids$mean_elevation[i] <- elevation |> summarise(Elevation = mean(Elevation)) |> pull(Elevation)
    all_els[[i]] <- elevation
  })
}

elevations <- bind_rows(all_els)
grids <- grids |> select(GridID, GridScale, MeanElevation=mean_elevation, Centroid, Area, geometry)
save(grids, countries, elevations, file="~/Documents/GitHub/weatherscrape/notebooks/grids_with_el.rda")
(load("~/Documents/GitHub/weatherscrape/notebooks/grids_with_el.rda"))

theme_set(theme_light())
pdf("notebooks/elevations.pdf", height=10, width=15)
grids |>
  filter(GridScale == "100x100km") |>
  ggplot(aes(geometry=geometry, fill=MeanElevation)) +
  geom_sf()

grids |>
  filter(GridScale == "10x10km") |>
  ggplot(aes(geometry=geometry, fill=MeanElevation)) +
  geom_sf()

ggplot(elevations, aes(geometry=geometry, col=Elevation)) +
  geom_sf()
dev.off()

