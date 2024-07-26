## ----setting------------------------------------------------------------------
#--Install / load packages
rm(list = ls())
pacman::p_load(sf, here, tmap, osmdata, tidyverse, data.table, rio, tidyverse, flextable, mapview, units, spdep, deldir, sp, rgeoda, leaflet, viridis, crosstalk, leaflet.extras, plotly)

#--Import street-level crime data
crime <- rio::import(here::here("3_output", "crime_2024-05-09.csv")) |>
    dplyr::mutate(category = stringr::str_replace_all(category, "-", " ")) |>
    sf::st_as_sf(coords = c("location.longitude", "location.latitude"), crs = 4326, dim = "XY") 
    #from 2021-04 to 2024-03

#--Import Barnet shapefile
bnt_shp <- sf::st_read(here("1_data", "9_geo", "bnt_lad.json"), crs = 4326, quiet = TRUE) |>
  st_make_valid()

#--Filter crime that intersects or is in within Barnet file
#crime_bnt <- crime[which(st_covers(bnt_shp, crime, sparse = FALSE)),]
crime_bnt <- crime[which(st_intersects(bnt_shp, crime, sparse = FALSE)),]

#--Amend date column
crime_bnt$date <- as.Date(paste0(crime_bnt$month, "-01"))

#--Create shared_data
shared_data <- SharedData$new(crime_bnt)


## ----m_eda--------------------------------------------------------------------
#--Assign colour palette
n_pal <- length(unique(crime_bnt$category))
crime_pal <- leaflet::colorFactor(turbo(n_pal), crime_bnt$category)

#--Create filters
month_slider <- crosstalk::filter_slider("date", "Date", shared_data, ~date, width = "100%")
category_checkbox <- crosstalk::filter_checkbox("category", "Category", shared_data, ~category)

#--Create map
m_eda <- leaflet(shared_data) |>
  leaflet::addProviderTiles("CartoDB.Positron")|>
  leaflet::addCircleMarkers(color = ~crime_pal(category), radius = 2) |>
  leaflet::addLegend("bottomright", pal = crime_pal, values = ~category, title = "Category")

#--Pull everything together
crosstalk::bscols(
  widths = c(12, 3, 9),
  month_slider,
  category_checkbox,
  m_eda
)


## -----------------------------------------------------------------------------
#--Create timeseries data by category
ct_crime <- crime_bnt |>
    st_drop_geometry() |>
    group_by(category, date) |>
    tally() |>
    arrange(desc(n))

#--Create shared_data
shared_data_ct <- SharedData$new(ct_crime)

#--Create filter
month_slider_ct <- crosstalk::filter_slider("date", "Date", shared_data_ct, ~date, step = 1, width = "100%")

#--Trend plot
pl_trend <- plotly::plot_ly(shared_data_ct, x = ~date, y = ~n, color = ~category, colors = viridis_pal(option = "H")(14)) |>
  plotly::add_lines() |>
  plotly::layout(xaxis = list(title = ""),
                 yaxis = list(title = "Number of Crimes\n"))

#--Pull everything together
crosstalk::bscols(
  widths = c(12, 12),
  month_slider_ct,
  pl_trend
)


## -----------------------------------------------------------------------------
#--Create count data by category
ct_crime2 <- crime_bnt |>
    st_drop_geometry() |>
    group_by(category) |>
    tally() |>
    arrange(desc(n)) 

#--Frequency plot
pl_freq <- plot_ly(ct_crime2, x = ~stats::reorder(category, n, decreasing = TRUE), y = ~n, color = ~category, colors = viridis_pal(option = "H")(14)) |>
  plotly::add_bars()|>
  plotly::layout(xaxis = list(title = ""),
                 yaxis = list(title = "Number of Crimes\n"))

pl_freq


## -----------------------------------------------------------------------------
#--Create count data by category in the last 12 months
ct_crime3 <- crime_bnt |>
    st_drop_geometry() |>
    filter(date >= ymd("2023-04-01")) |>
    group_by(category) |>
    tally() |>
    arrange(desc(n)) 

#--Frequency plot
pl_freq2 <- plot_ly(ct_crime3, x = ~stats::reorder(category, n, decreasing = TRUE), y = ~n, color = ~category, colors = viridis_pal(option = "H")(14)) |>
  plotly::add_bars()|>
  plotly::layout(xaxis = list(title = ""),
                 yaxis = list(title = "Number of Crimes\n"))

pl_freq2


## -----------------------------------------------------------------------------
#--Import places data 
source(here("2_script", "osm_process.r"))
  # should have poi_bnt_fin after loading

#--Check result
#---Create map for each POI
#----Initialise empty list
l_m_poi <- vector("list", length(poi_bnt_fin))

#----Name the list
names(l_m_poi) <- names(poi_bnt_fin)

#---Loop the map making function
for (i in seq_along(poi_bnt_fin)){
  l_m_poi[[i]] <- leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolygons(data = bnt_shp, fillOpacity = 0, color = "black") |>
    leaflet::addCircleMarkers(data = poi_bnt_fin[[i]], radius = 2) |>
    leaflet::leafletOptions()
}

#---View maps
l_m_poi[["pub"]]


## -----------------------------------------------------------------------------
#--Create a table for the count of POI
poi_df <- data.frame(
  Place = names(poi_bnt_fin),
  Count = as.numeric(map(poi_bnt_fin, ~nrow(.x)))
) |>
 arrange(desc(Count)) |>
 mutate(Index = 1:length(poi_bnt_fin), .before = everything())

#--Convert the table into flextable
poi_df |> 
  flextable::flextable() |>
  flextable::hline()

