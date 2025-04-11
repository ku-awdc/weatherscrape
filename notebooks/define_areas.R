library("tidyverse")
library("hexscape")
library("sf")
library("units")

all_nuts_codes(level=0L) |>
  pull(NUTS) |>
  load_map() ->
  cc

all_nuts_codes(level=2) |>
  filter(str_detect(NUTS, "NO"), NUTS!="NO0B") |>
  pull(NUTS) |>
  load_map() |>
  mutate(NUTS = str_sub(NUTS,1,2)) |>
  group_by(NUTS) |>
  summarise() |>
  ungroup() ->
  norway

norway |>
  ggplot(aes(fill=NUTS)) +
  geom_sf()

all_nuts_codes(level=1) |>
  filter(str_detect(NUTS, "FR") | str_detect(NUTS, "ES") | str_detect(NUTS, "PT")) |>
  filter(!NUTS %in% c("FRY","ES7","PT2","PT3")) |>
  pull(NUTS) |>
  load_map() |>
  mutate(NUTS = str_sub(NUTS,1,2)) |>
  group_by(NUTS) |>
  summarise() |>
  ungroup() ->
  es_fr_pt

all_nuts_codes(level=1) |>
  filter(str_detect(NUTS, "FR"), NUTS!="FRY") |>
  pull(NUTS) |>
  load_map() |>
  ggplot(aes(fill=NUTS)) +
  geom_sf()

all_nuts_codes(level=1) |>
  filter(str_detect(NUTS, "ES"), NUTS!="ES7") |>
  pull(NUTS) |>
  load_map() |>
  ggplot(aes(fill=NUTS)) +
  geom_sf()

all_nuts_codes(level=1) |>
  filter(str_detect(NUTS, "PT"), NUTS!="PT2", NUTS!="PT3") |>
  pull(NUTS) |>
  load_map() |>
  ggplot(aes(fill=NUTS)) +
  geom_sf()


library("rnaturalearth")
library("rnaturalearthhires")  # To install, just run the code below

mct <- c("Andorra", "Belarus", "Bosnia and Herzegovina", "Moldova", "Monaco", "Ukraine", "Vatican City", "Kosovo", "San Marino")

ne_countries(scale = "large", returnclass = "sf") |>
  filter(name_en %in% mct) |>
  mutate(NUTS = sov_a3, Label = str_c(name_en, " (ne)")) |>
  select(NUTS, Label, geometry) |>
  st_transform(st_crs(norway)) ->
  rne
stopifnot(nrow(rne)==length(mct))

# Note: there is an overlap:
st_intersection(cc, rne) |>
  ggplot() + geom_sf(aes(fill=NUTS))

cc |>
  filter(!NUTS %in% c("NO","FR","ES","PT")) |>
  bind_rows(
    norway,
    es_fr_pt,
    rne
  ) |>
  select(-Level) |>
  mutate(Label = case_when(
    NUTS == "NO" ~ "Norway (-NO0B)",
    NUTS == "FR" ~ "France (-FRY)",
    NUTS == "ES" ~ "Spain (-ES7)",
    NUTS == "PT" ~ "Portugal (PT1 only)",
    TRUE ~ Label
  )) ->
  countries

countries |>
  filter(NUTS %in% c("IT")) |>
  st_bbox() ->
  bb

countries |>
  ggplot() +
  geom_sf(aes(fill=NUTS)) +
#  coord_sf(ylim=bb[c(2,4)], xlim=bb[c(1,3)]) +
  theme_void()



ne_countries(scale = "large", returnclass = "sf") |>
  filter(name_en %in% "Russia") |>
  mutate(NUTS = sov_a3, Label = str_c(name_en, " (ne)")) |>
  select(NUTS, Label, geometry) |>
  st_transform(st_crs(norway)) |>
  st_intersection(st_as_sfc(st_bbox(countries))) ->
  rus

bind_rows(countries, rus) |>
  ggplot() +
  geom_sf(aes(fill=NUTS)) +
  coord_sf(datum = "EPSG:3035")


# Extract Kaliningrad and Crimea from Russia:
klgbx <- st_point(c(5050000,3550000)) |> st_buffer(200000) |> st_sfc(crs=st_crs(rus))
crmbx <- st_union(
  st_point(c(6350000,2840000)) |> st_buffer(12000) |> st_sfc(crs=st_crs(rus)),
  st_point(c(6200000,2800000)) |> st_buffer(162000) |> st_sfc(crs=st_crs(rus))
)
ggplot() +
  geom_sf(data=rus) +
  geom_sf(data=klgbx, col="blue", fill="transparent") +
  geom_sf(data=crmbx, col="red", fill="transparent") +
  coord_sf(datum = "EPSG:3035") +
  #coord_sf(xlim=c(6000000,6500000), ylim=c(2500000,3000000), datum = "EPSG:3035") +
  #coord_sf(xlim=c(6300000,6400000), ylim=c(2800000,2900000), datum = "EPSG:3035") +
  theme_light()

crimea <- st_intersection(rus, crmbx)
kaliningrad <- st_intersection(rus, klgbx)

ggplot() +
  geom_sf(data=rus) +
  geom_sf(data=kaliningrad, fill="blue") +
  geom_sf(data=crimea, fill="red") +
  coord_sf(datum = "EPSG:3035") +
  #coord_sf(xlim=c(6000000,6500000), ylim=c(2500000,3000000), datum = "EPSG:3035") +
  #coord_sf(xlim=c(6300000,6400000), ylim=c(2800000,2900000), datum = "EPSG:3035") +
  theme_light()

countries <- bind_rows(countries,
                       crimea |> mutate(NUTS = "CRI", Label = "Crimea (ne)"),
                       kaliningrad |> mutate(NUTS = "RUS", Label = "Kaliningrad (ne)")
                       )

bb <- st_bbox(countries)
xs <- seq(floor(bb[1]/1e5), ceiling(bb[3]/1e5), by=1)
ys <- seq(floor(bb[2]/1e5), ceiling(bb[4]/1e5), by=1)

expand_grid(
  xmax = xs[-1L],
  ymax = ys[-1L]
) |>
  mutate(xmin = xmax-1, ymin = ymax-1) |>
  rowwise() |>
  group_split() |>
  map(\(x){
    tb <- bb
    tb[] <- as.numeric(x[names(bb)]) * 10^5
    x |>
      mutate(geometry = st_as_sfc(tb))
  }, .progress=TRUE) |>
  bind_rows() |>
  filter(st_intersects(geometry, st_union(countries), sparse=FALSE)[,1]) |>
  mutate(geometry = st_intersection(geometry, st_union(countries))) |>
  st_as_sf() |>
  mutate(GridID = str_c("E",xmin,"N",ymin)) |>
  mutate(GridScale = "100x100km", Centroid = st_centroid(geometry, of_largest_polygon = TRUE), Area = st_area(geometry)) |>
  select(GridID, GridScale, Centroid, Area, geometry) ->
  grids_100

bb <- st_bbox(countries |> filter(NUTS=="DK"))
xs <- seq(floor(bb[1]/1e4), ceiling(bb[3]/1e4), by=1)
ys <- seq(floor(bb[2]/1e4), ceiling(bb[4]/1e4), by=1)

expand_grid(
  xmax = xs[-1L],
  ymax = ys[-1L]
) |>
  mutate(xmin = xmax-1, ymin = ymax-1) |>
  rowwise() |>
  group_split() |>
  map(\(x){
    tb <- bb
    tb[] <- as.numeric(x[names(bb)]) * 10^4
    x |>
      mutate(geometry = st_as_sfc(tb))
  }, .progress=TRUE) |>
  bind_rows() |>
  filter(st_intersects(geometry, st_union(countries |> filter(NUTS=="DK")), sparse=FALSE)[,1]) |>
  mutate(geometry = st_intersection(geometry, st_union(countries |> filter(NUTS=="DK")))) |>
  st_as_sf() |>
  mutate(GridID = str_c("E",xmin,"N",ymin)) |>
  mutate(GridScale = "10x10km", Centroid = st_centroid(geometry, of_largest_polygon = TRUE), Area = st_area(geometry)) |>
  select(GridID, GridScale, Centroid, Area, geometry) ->
  grids_10

grids <- bind_rows(grids_10, grids_100)
#save(grids, countries, file="notebooks/grids.rda")


(load("notebooks/grids.rda"))

ggplot() +
  geom_sf(data=countries, aes(fill=NUTS), alpha=0.15) +
  geom_sf(data=grids |> filter(GridScale=="100x100km"), fill="transparent") +
  geom_sf(data=grids |> filter(GridScale=="100x100km"), aes(geometry=Centroid), size=0.5) +
  theme_void() +
  theme(legend.position = "none")
ggsave("notebooks/country_grids.pdf", width=12, height=10)

ggplot() +
  geom_sf(data=countries |> filter(NUTS=="DK"), aes(fill=Label), alpha=0.15) +
  geom_sf(data=grids |> filter(GridScale=="10x10km"), fill="transparent") +
  geom_sf(data=grids |> filter(GridScale=="10x10km"), aes(geometry=Centroid), size=0.5) +
  theme_void() +
  theme(legend.position = "none")
ggsave("notebooks/denmark_grids.pdf", width=12, height=10)


ggplot() +
  geom_sf(data=countries, aes(fill=NUTS), alpha=0.15) +
  theme_void() +
  theme(legend.title=element_blank())
ggsave("notebooks/countries.pdf", width=12, height=10)
