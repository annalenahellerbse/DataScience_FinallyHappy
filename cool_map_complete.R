# Load required libraries
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)


finally_happy <- read.csv("https://raw.githubusercontent.com/annalenahellerbse/DataScience_FinallyHappy/refs/heads/main/finally_happy_abs.csv")

# Preprocess the data
# Filter the data to only include rows from 2022
data_2022 <- finally_happy %>%
  filter(year == 2022)

# Add ISO country codes based on country names
data_2022$iso_a3 <- countrycode(data_2022$country, origin = "country.name", destination = "iso3c")

# Add ISO-3 code manually for Kosovo
data_2022 <- data_2022 %>%
  mutate(
    iso_a3 = ifelse(country == "Kosovo", "XKX", iso_a3)
  )

# Load world shapefile data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Replace -99 with XKX in the iso_a3_eh column
world <- world %>%
  mutate(iso_a3_eh = ifelse(iso_a3_eh == "-99", "XKX", iso_a3_eh))


# Merge inequality data with the world shapefile
world_data <- world %>%
  left_join(data_2022, by = c("iso_a3_eh" = "iso_a3"))

# Create a color palette for income inequality
pal <- colorNumeric(
  palette = "RdYlGn", # Red for high values, green for low values
  domain = data_2022$income_inequ_top10,
  reverse = TRUE # Reverse to make red represent high inequality
)

# Create the interactive map
leaflet(world_data) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(income_inequ_top10),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9),
    label = ~paste0(name, ": ", round(income_inequ_top10, 2))
  ) %>%
  addLegend(
    pal = pal,
    values = ~income_inequ_top10,
    title = "Income Inequality (Top 10%)",
    position = "bottomright"
  ) %>%
  addControl(
    "<h3>Global Income Inequality Map (Top 10%) - 2022</h3>",
    position = "topright"
  )
