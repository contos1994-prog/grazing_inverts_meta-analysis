# Assign a RESOLVE Ecoregions 2017 biome (Dinerstein et al. 2017, BioScience 67:534)
# to every study site in MetaAnalysis_data_v2.xlsx.
# Run from the repo root:  Rscript scripts/01_extract_biomes.R [path/to/Ecoregions2017.shp]
#
# The polygons are the RESOLVE Ecoregions 2017 layer (CC BY 4.0), the same data
# served on ArcGIS Online as "Biomes" (item 0fccc890adc74032acc60ff7a66005f7).
# The public ArcGIS query endpoint is rate-limited and shared, so the shapefile
# is downloaded once (~150 MB, not committed) and points are matched locally.
#
# Each unique Study_ID x (lat, lon) is a point. Points outside every polygon
# (coastal sites, coordinate rounding) take the nearest ecoregion if it is
# within ~25 km. Studies with no coordinates in the data are given approximate
# coordinates from their site description (MANUAL below), flagged in
# coord_source / coord_note. Writes data/study_biomes.csv, read by R/04_biome.R.

suppressPackageStartupMessages({ library(sf); library(readxl); library(dplyr) })
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
shp <- if (length(args)) args[1] else file.path(tempdir(), "Ecoregions2017", "Ecoregions2017.shp")
if (!file.exists(shp)) {
  zip <- file.path(tempdir(), "Ecoregions2017.zip")
  download.file("https://storage.googleapis.com/teow2016/Ecoregions2017.zip", zip, mode = "wb")
  unzip(zip, exdir = dirname(shp))
}
eco <- st_read(shp, quiet = TRUE) |> select(BIOME_NAME, BIOME_NUM, ECO_NAME, REALM) |>
  filter(BIOME_NAME != "N/A")

# approximate site coordinates for studies with none in the data
MANUAL <- tribble(
  ~Study_ID, ~lat,   ~lon,     ~note,
  "#1796",   52.35,  0.30,     "approximate: tall-herb fen, England, exact site not given (East Anglian fens assumed)",
  "#2260",   57.0,   -4.0,     "approximate: centroid of 8 Scottish native pinewoods",
  "#1825",   60.0,   16.0,     "approximate: 16 stands across three latitudes in Sweden; mid-latitude point used, ambiguous",
  "#1349",   41.88,  -89.34,   "approximate: Nachusa Grasslands, Illinois",
  "#1589",   31.59,  -110.51,  "approximate: Appleton-Whittell Research Ranch, Arizona",
  "#3610",   31.59,  -110.51,  "approximate: Appleton-Whittell Research Ranch, Arizona",
  "#1048",   39.13,  -84.25,   "approximate: Cincinnati Nature Center, Ohio",
  "#2091",   39.20,  -76.88,   "approximate: Middle Patuxent Environmental Area, Maryland",
  "#1278",   29.58,  101.88,   "approximate: Gongga Mountain alpine station, Sichuan",
  "#2189",   43.85,  -102.25,  "approximate: Badlands National Park North Unit, South Dakota",
  "#1609",   40.0,   -3.7,     "approximate: Spain, site not given (central Spain assumed), ambiguous",
  "#1503",   53.07,  -4.05,    "approximate: Llyn Llydaw, Snowdonia",
  "#2386",   37.1,   -3.0,     "approximate: SE Spain montane (Sierra Nevada assumed)")

raw <- read_excel("MetaAnalysis_data_v2.xlsx", sheet = "Invertebrates", guess_max = 5000, .name_repair = "unique_quiet")
sites <- tibble(Study_ID = trimws(raw$Study_ID),
                Latitude_decimal = suppressWarnings(as.numeric(raw$Latitude_decimal)),
                Longitude_decimal = suppressWarnings(as.numeric(raw$Longitude_decimal)),
                Biomes_as_entered = raw$Biomes) |>
  filter(!is.na(Study_ID)) |>
  distinct(Study_ID, Latitude_decimal, Longitude_decimal, .keep_all = TRUE) |>
  left_join(MANUAL, by = "Study_ID") |>
  mutate(coord_source = ifelse(!is.na(Latitude_decimal), "data", ifelse(!is.na(lat), "manual", "none")),
         query_lat = coalesce(Latitude_decimal, lat), query_lon = coalesce(Longitude_decimal, lon),
         coord_note = ifelse(coord_source == "manual", note, "")) |>
  select(-lat, -lon, -note)

q <- sites |> filter(!is.na(query_lat))
pts <- st_as_sf(q, coords = c("query_lon", "query_lat"), crs = 4326, remove = FALSE)
hit <- st_intersects(pts, eco)
idx <- sapply(hit, function(h) if (length(h)) h[1] else NA_integer_)
match <- ifelse(is.na(idx), NA, "point in polygon")
for (i in which(is.na(idx))) {
  near <- st_nearest_feature(pts[i, ], eco)
  km <- as.numeric(st_distance(st_transform(pts[i, ], 3857), st_transform(eco[near, ], 3857))) / 1000 *
        cos(q$query_lat[i] * pi / 180)          # rough Web-Mercator scale correction
  if (km <= 25) { idx[i] <- near; match[i] <- sprintf("nearest ecoregion, ~%.0f km", km) }
  else match[i] <- sprintf("no ecoregion within 25 km (nearest ~%.0f km)", km)
}
att <- st_drop_geometry(eco)[idx, ]
q <- bind_cols(q, match = match, att)

out <- sites |> left_join(q |> select(Study_ID, Latitude_decimal, Longitude_decimal, match, BIOME_NAME, BIOME_NUM, ECO_NAME, REALM),
                          by = c("Study_ID", "Latitude_decimal", "Longitude_decimal")) |>
  mutate(match = ifelse(coord_source == "none", "no coordinates", match)) |>
  select(Study_ID, Latitude_decimal, Longitude_decimal, query_lat, query_lon, coord_source, coord_note,
         match, BIOME_NAME, BIOME_NUM, ECO_NAME, REALM, Biomes_as_entered) |>
  arrange(Study_ID)

dir.create("data", showWarnings = FALSE)
write.csv(out, "data/study_biomes.csv", row.names = FALSE)
message(sprintf("wrote data/study_biomes.csv: %d study-sites, %d with a biome", nrow(out), sum(!is.na(out$BIOME_NAME))))
