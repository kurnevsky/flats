library(geosphere)
library(mice)
library(dplyr)
library(clue)

set.seed(7)

extend <- function(flats, stations, regions) {
  flats$distance_to_subway <- apply(
    distm(
      flats[, c("longitude", "latitude")],
      stations[, c("longitude", "latitude")],
      fun = distHaversine
    ),
    1,
    min
  )
  flats$first_or_last_floor <- flats$floor == 1 | flats$floor == flats$total_floors
  flats$other_area <- flats$total_area - flats$living_area - flats$kitchen_area
  return(flats)
}

impute <- function(flats, my_flat) {
  imp_flats <- as.data.frame(bind_rows(flats, my_flat))
  imp_flats <- imp_flats[, c(
    "rooms",
    "total_floors",
    "living_area",
    "kitchen_area",
    "other_area",
    "house_type",
    "year",
    "balcony",
    "ceiling_height",
    "distance_to_subway",
    "region"
  )]
  imp_flats_mids <- mice(imp_flats, m = 1, maxit = 100)
  imp_flats <- complete(imp_flats_mids)
  imp_flats <- imp_flats[-nrow(imp_flats), ]
  flats$year <- imp_flats$year
  flats$ceiling_height <- imp_flats$ceiling_height
  return(flats)
}

flats <- read.csv(file = "onliner-2020-11-21T18:46:21.csv", head = TRUE, sep = ",", na.strings = c(""))

flats$resale <- flats$resale == 1
flats$house_type <- factor(flats$house_type)
flats$balcony <- flats$balcony == 1
flats$parking <- factor(flats$parking)
flats$cottage <- flats$cottage == 1
flats$actual <- flats$actual == 1

flats <- flats[flats$actual, ]
flats <- flats[!flats$cottage, ]
flats <- flats[
  flats$latitude > 53.75480084404933 &
    flats$latitude < 54.04165104090459 &
    flats$longitude > 27.321624755859375 &
    flats$longitude < 27.802276611328125,
]
flats <- flats[flats$rooms == 2, ]
flats <- flats[flats$resale, ]

flats <- flats[flats$price > 30000 & flats$price < 100000, ]

my_flat <- read.csv(file = "my_flat.csv", head = TRUE, sep = ",", na.strings = c(""))
my_flat$resale <- my_flat$resale == 1
my_flat$house_type <- factor(my_flat$house_type, levels = levels(flats$house_type))
my_flat$balcony <- my_flat$balcony == 1
my_flat$parking <- factor(my_flat$parking, levels = levels(flats$parking))

stations <- read.csv(file = "stations.csv", head = TRUE, sep = ",")

clusters <- kmeans(flats[, c("latitude", "longitude")], 32, iter.max = 1000)
regions <- data.frame(clusters$centers)
regions$name <- factor(seq_len(nrow(regions)))
flats$region <- factor(clusters$cluster)
my_flat$region <- factor(cl_predict(clusters, my_flat[, c("latitude", "longitude")]))

flats <- extend(flats, stations, regions)
my_flat <- extend(my_flat, stations, regions)

flats <- impute(flats, my_flat)

model <- lm(
  price ~
  living_area +
    kitchen_area +
    other_area +
    house_type +
    year +
    balcony +
    ceiling_height +
    distance_to_subway +
    first_or_last_floor +
    region,
  data = flats
)

outliers <- sort(abs(rstudent(model)), decreasing = TRUE)[1:10]
flats <- flats[!rownames(flats) %in% names(outliers), ]

model <- lm(
  price ~
  living_area +
    kitchen_area +
    other_area +
    house_type +
    year +
    balcony +
    ceiling_height +
    distance_to_subway +
    first_or_last_floor +
    region,
  data = flats
)

predict(model, my_flat)

summary(model)



library(leaflet)
library(htmltools)
library(htmlwidgets)

coordinates_plugin <- htmlDependency(
  "leaflet.coordinates",
  "0.1.3",
  src = c(href = "file:///home/kurnevsky/Downloads/"),
  script = "Leaflet.Coordinates-0.1.5.min.js",
  stylesheet = "Leaflet.Coordinates-0.1.5.css"
)

register_plugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

colors <- colorFactor(rainbow(nrow(regions), alpha = NULL), regions$name)

options(browser = "firefox")

leaflet(data = flats) %>%
  addTiles() %>%
  addCircles(
    lat = regions$latitude,
    lng = regions$longitude,
    radius = 1000,
    stroke = FALSE,
    fillOpacity = 0.5,
    color = colors(regions$name)
  ) %>%
  addLegend("topright", colors = colors(regions$name), labels = regions$name) %>%
  addCircleMarkers(
    lat = ~latitude,
    lng = ~longitude,
    radius = 7,
    stroke = FALSE,
    fillOpacity = 1,
    color = ~ colors(region),
    label = ~price,
    popup = paste0(
      "rooms: ",
      flats$rooms,
      "<br>floor: ",
      flats$floor,
      "<br>year: ",
      flats$year,
      "<br>total area: ",
      flats$total_area,
      "<br>living area: ",
      flats$living_area,
      "<br>created: ",
      flats$created
    )
  ) %>%
  register_plugin(coordinates_plugin) %>%
  onRender("function(el, x) {
    L.control.coordinates({
      decimals: 5,
      enableUserInput: false,
      useLatLngOrder: true,
      labelTemplateLat: 'Latitude: {y},',
      labelTemplateLng: 'Longitude: {x}'
    }).addTo(this);
  }")
