library("hexscape")
library("sf")

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
  #  coord_sf(ylim=bb[c(2,4)], xlim=bb[c(1,3)]) +
  theme_void()

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
  mutate(geometry = st_intersection(geometry, st_union(bind_rows(countries, rus)))) |>
  st_as_sf() ->
  boxes

ggplot() +
  geom_sf(data=bind_rows(countries,rus), aes(fill=Label), alpha=0.15) +
  geom_sf(data=boxes, fill="transparent") +
  geom_sf(data=boxes |> mutate(pt = st_centroid(geometry, of_largest_polygon = TRUE)), aes(geometry=pt), size=0.5) +
  theme_void() +
  theme(legend.position = "none")
ggsave("weather_grid.pdf", width=12, height=10)
