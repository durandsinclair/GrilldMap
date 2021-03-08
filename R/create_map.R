# DISTANCES TO GRILLD STORES

# Calcalates the distance to Grilld stores, based on tutorial from Amit 
# Levinson.
# https://github.com/AmitLevinson/amitlevinson.com/blob/master/content/post/ice-cream-locations/index.Rmd

# Based on a different tutorial on the distance to the sea in Iceland. https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/

# 0) PREPARE ENVIRONMENT --------------------------------------------------

    # Clear global environment
    rm(list = ls())
    gc()
    cat("\014")
    
    # Load libraries
    library(tidyverse)
    library(leaflet)
    
    library(htmltools)
    library(sf)
    library(ggspatial)
    library(ggrepel)
    library(reactable)
    library(ggtext)
    library(extrafont)
    library(patchwork)
    library(glue)

# 1) GET DATA -------------------------------------------------------------

    locations <- read_csv(here::here("data",
                                     "processed",
                                     "sydney.csv"
                                     ) 
                          ) #%>%
        mutate(id = 1:nrow(.), .before = name) %>%
        as_tibble()
    a <- locations %>%
        select(address = address...7 ) 
    a
    b <- a %>%
        separate(
            address,
            c("street", "country"), 
            ", australia"
        ) %>%
        separate(
            street,
            c("one", "two", "three"),
            ", "
        )
    b
        stringr::str_split_fixed(
        a$address...7,
        pattern = ", australia", 
        n = 2
    )
    b %>% dplyr::row_number(12)
# 2) VISUALISE DATA -------------------------------------------------------

    # Label Function
    make_label <- function(x, y, n){
        glue("<p style='text-align:right;font-family:Calibri;font-size:12px;'>
    <b>{x}</b></br>
  {y}<br/>
  <span style='color:#808080;'>{n}</p>") %>% 
            HTML()
    }
    
    # Create labels
    burger_labels <- pmap(
        list(locations$name, locations$address...7, locations$phone),
        make_label
    )
    # Use an icon for points
    burger_icon <-  makeIcon("https://www.grilld.com.au/images/logo-grilld-love.svg",
                             iconWidth = 36, 
                             iconHeight = 36
                             )
    #burger_icon <-  makeIcon("https://www.grilld.com.au/images/logo-grilld.png", 
    #                         iconWidth = , 
    #                         iconHeight = 24
    #                         )
    
    # Create map
    leaflet(data = locations) %>%
        addTiles() %>%
        addMarkers(data = locations, 
                   icon = burger_icon, 
                   label = ~ burger_labels)
    

# 3) MEASURING DISTANCES -------------------------------------------------
    
    # Convert the locations in WGS Co-ordinate reference system
    burger_projected <- locations %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
    

    # Also convert to the metric system
    burger_meters <- st_transform(burger_projected, crs = 32755)
    
    
    # Get a map of Sydney
    syd_map_sf <- st_transform(
        st_read(
            here::here("data", "interim", "GCCSA_2016_AUST.shp"),
            quiet = TRUE
        ), 4326
    ) %>%
        filter(GCC_NAME16 == "Greater Sydney")
        
    syd_map_metric <- st_transform(
        st_read(
            here::here("data", "interim", "GCCSA_2016_AUST.shp"),
            quiet = TRUE
        ), 
        crs = 32755,
        coords = c("lon", "lat")
    ) %>% filter(GCC_NAME16 == "Greater Sydney")
    
    # Commented out for speed next time
    # new_syd <- st_make_grid(syd_map_metric, cellsize = 2000)
    # grid <- st_intersection(new_syd, syd_map_metric)
    # grid_wgs <- st_transform(grid, crs = 4326)
    # saveRDS(object = grid, file = here::here("data", "processed", "grid.rds"))
    # saveRDS(object = grid_wgs, file = here::here("data", "processed", "grid_wgs.rds"))
    
    # Load the above objects as rds for faster rendering
    grid <- readRDS(
        here::here(
            "data",
            "processed",
            "grid.rds"
        )
    )

    grid_wgs <- readRDS(
        here::here(
            "data",
            "processed",
            "grid_wgs.rds"
            )
        )
    
    # Visualise Map
    theme_set(theme_void()) +
                  theme(
                      # text = element_text("IBM Plex Sans"),
                      plot.title = element_text(color = "#0C0C44",
                                                face = "bold")
                      )
    
    
    # p1 <- ggplot(syd_map_metric) + 
    p1 <- ggplot(syd_map_sf) + 
        geom_sf()
    
    p2 <- ggplot(grid) + 
        geom_sf()
    
    p1 + 
        p2 +
        plot_annotation(
            title = "Map of Sydney (left) and map cut into 2km<sup>2</sup> areas (right)",
            theme = theme(plot.title = element_markdown(size = 12))
        )
    #

# DISTANCE FROM STORE ----
    
    # Create a sample dataframe
    dataum <- data.frame(
        geometry = st_geometry(burger_projected[8:13,]),
        #location_grid = st_centroid(rep(grid_wgs[350,], 6))
        location_grid = st_centroid(rep(grid_wgs[1500,], 6))
    )
    
    # Our data points projected to Sydney's CRS (32755)
    data_lines <- map_dfc(dataum, ~ st_transform(.x, crs=32755))
    
    # Our centroid, projected to Sydney's CRS (32755)
    sample_data <- data_lines %>%
        
        # Convert back to 4326 CRS
        map_dfc(~ st_transform(.x, crs = 4326) %>% 
                    st_coordinates(.x)) %>%
        map_dfc(as.data.frame) %>%
        
        # Rename the coordinates
        set_names(c("burger.x", "burger.y", "us.x", "us.y")) %>%
        
        # Add the distances
        cbind(distance = map2_dbl(data_lines$geometry, 
                                  data_lines$geometry.1, 
                                  st_distance),
              location_polygon = st_transform(data_lines$geometry.1, 
                                              crs = 4326)) %>%
        
        # Figure out which store has the minimum distance.
        mutate(relevant = ifelse(distance==min(distance), "yes", "no"),
               distance = ifelse(relevant=="yes", 
                                 paste0(round(distance/1000, 0), "km"),
                                 round(distance/1000, 0)))

    # POINT LABELS ???
    point_labels <- data.frame(x = c(150.05, 150.7977, 151.1195),
                               y = c(-34.15968, -34.07338, -33.7766),
                               label = c("Example grid", 
                                         "Nearest Grilld", 
                                         "Grilld Locations")
                               )
    
    # Show distances from a point to various restaurants
    ggplot(syd_map_sf) + 
        geom_sf() + 
        
        # Plot restaurants as points
        geom_point(data = sample_data,
                   mapping = aes(x = burger.x, 
                                 y = burger.y, 
                                 color = relevant),
                   size = 1.2) +
        
        # Plot grid square we want to measure from. (Eg Bankstown airport)
        geom_sf(data = grid_wgs[1500,], 
                aes(geometry = geometry), 
                fill = "red", 
                color = "red",
                size = 1.5
                ) +
        
        # Draw lines from grid square to restaurant
        geom_spatial_segment(data = sample_data, 
                             mapping = aes(x = us.x, 
                                           xend = burger.x, 
                                           y = us.y, 
                                           yend = burger.y, 
                                           linetype = relevant, 
                                           color = relevant), 
                             show.legend = FALSE,
                             crs = 4326) +
        
        # Add labels showing distance from restaurant to starting location
        geom_spatial_text_repel(data = sample_data,
                                mapping = aes(x = burger.x, 
                                              y = burger.y, 
                                              label = distance, 
                                              color = relevant), 
                                crs = 4326, hjust = 0.5, size = 3.5) +
        
        
        # Add grey labels for "Grilld Locations", "Nearest Grilld" and "Example Grid" 
        geom_text_repel(data = point_labels, 
                        mapping = aes(x = x, y = y, label = label), 
                        xlim = c(151, 152),
                        point.padding = 0.1, 
                        arrow = arrow(length =unit(0.015, "npc"),
                                      type = "closed"),
                        segment.linetype = 8, color = "gray55",
                        segment.color = "gray80", 
                        size = 3) +
        
        # Turn the lines grey, and either dashed or dotted
        scale_color_manual(values = c("yes" = "red", "no" = "gray60")) +
        scale_linetype_manual(values = c("dashed", "solid")) +
    
        
        xlim(150, 152) + 
        coord_sf(clip = "off") +
        guides(color = "none") +
        labs(title = "Finding the nearest Grilld lcoation for each grid") +
        theme(plot.title = element_text(size = 13, hjust = 0.5))
    
    
# WHERE'S OUR ICE CREAM -------------
    
    # Create a dataframe where each column is a grid location
    #   and each row is a restaurant, and each cell is a distance between
    #   the two
    distances <- st_distance(burger_meters, st_centroid(grid)) %>%
        as_tibble()
    
    burger_distances <- data.frame(
        
        # We want grids in a WGS 84 CRS:
        us = st_transform(grid, crs = 4326),
        
        # Extract minimum distance for each grid
        distance_km = map_dbl(distances, min) / 1000,
        
        # Extract the value's index for joining with the burger location 
        location_id = map_dbl(distances, function(x) match(min(x), x))
    ) %>%
        
        # Join with the burger table
        left_join(burger_projected, by = c("location_id" = "id"))

# Bin ranges for a nice colour scale
bins <- c(0, 5, 15, 30, 50, 700)
# Binned colour palette
pal <- colorBin(c("#FF1554", "#FF3C70", "#FF7096", "#FFA4BC", "#FFE5EC"),
                domain = burger_distances$distance_km, 
                bins = bins, 
                reverse = TRUE
                )
make_label_distances <- function(km, street, city){
    glue("
  <div style='text-align:left;'>
  You are <span style='font-size:13px;'><b>{round(km, 1)}</b></span> km from the nearest location at:</div>
  <div style='text-align:right;'>
       {street}, {city}</div>"
         ) %>% 
        HTML()}

burger_labels <- pmap(list(burger_distances$distance_km,
                           burger_distances$street,
                           burger_distances$city), 
                      make_label_distances)
burger_icon <-  makeIcon("https://www.grilld.com.au/images/logo-grilld-love.svg",
                         iconWidth = 18, 
                         iconHeight = 18
)

full_map <- leaflet() %>% 
    addTiles() %>% 
    addMarkers(data = burger_projected, 
               icon = ~burger_icon, 
               group = "Burger locations") %>% 
    addPolygons(data = burger_distances[[1]], 
                fillColor = pal(burger_distances$distance_km), 
                fillOpacity = 0.8, 
                weight = 0,
                opacity = 1, 
                color = "transparent", 
                group = "Distances", 
                popup = burger_labels, 
                highlight = highlightOptions(weight = 2.5, 
                                             color = "#666", 
                                             bringToFront = TRUE, 
                                             opacity = 1),
                popupOptions = popupOptions(autoPan = FALSE,
                                            closeOnClick = TRUE, 
                                            textOnly = T)
                ) %>% 
    addLegend(pal = pal, 
              values = (burger_distances$distance_km), 
              opacity = 0.8, 
              title = "Distance (Km)", 
              position= "bottomright"
              ) %>% 
    addLayersControl(overlayGroups = c("Burger locations", "Distances"),
                     options = layersControlOptions(collapsed = FALSE))

# Save as a widget for faster loading later
full_map
htmlwidgets::saveWidget(full_map, "widgets/full_map.html")



# PLAY ABOUT -------------------------------------------------------------------


# Create a sample dataframe
dataum <- data.frame(
    geometry = st_geometry(burger_projected[1:19,]),
    #location_grid = st_centroid(rep(grid_wgs[350,], 6))
    location_grid = st_centroid(rep(grid_wgs[1500,], 6))
)

# Our data points projected to Sydney's CRS (32755)
data_lines <- map_dfc(dataum, ~ st_transform(.x, crs=32755))

# Our centroid, projected to Sydney's CRS (32755)
sample_data <- data_lines %>%
    
    # Convert back to 4326 CRS
    map_dfc(~ st_transform(.x, crs = 4326) %>% 
                st_coordinates(.x)) %>%
    map_dfc(as.data.frame) %>%
    
    # Rename the coordinates
    set_names(c("burger.x", "burger.y", "us.x", "us.y")) %>%
    
    # Add the distances
    cbind(distance = map2_dbl(data_lines$geometry, 
                              data_lines$geometry.1, 
                              st_distance),
          location_polygon = st_transform(data_lines$geometry.1, 
                                          crs = 4326)) %>%
    
    # Figure out which store has the minimum distance.
    mutate(relevant = ifelse(distance==min(distance), "yes", "no"),
           distance = ifelse(relevant=="yes", 
                             paste0(round(distance/1000, 0), "km"),
                             round(distance/1000, 0)))

# POINT LABELS ???
point_labels <- data.frame(x = c(150.05, 150.7977, 151.1195),
                           y = c(-34.15968, -34.07338, -33.7766),
                           label = c("Example grid", 
                                     "Nearest Grilld", 
                                     "Grilld Locations")
)

# Show distances from a point to various restaurants
ggplot(syd_map_sf) + 
    geom_sf() + 
    
    # Plot restaurants as points
    #geom_point(data = sample_data,
    #           mapping = aes(x = burger.x, 
    #                         y = burger.y, 
    #                         color = relevant),
    #           size = 1.2) +
    
    # Plot grid square we want to measure from. (Eg Bankstown airport)
    geom_sf(data = grid_wgs[1500,], 
            aes(geometry = geometry), 
            fill = "red", 
            color = "red",
            size = 1
    ) 

# TEST FULL MAP ------

full_map <- leaflet() %>% 
    addTiles() %>% 
    addMarkers(data = burger_projected, 
               icon = ~burger_icon, 
               group = "Burger locations") %>% 
    addPolygons(data = burger_distances[[1]], 
                fillColor = "#BDBDBD", #pal(burger_distances$distance_km), 
                fillOpacity = 0.8, 
                weight = 0,
                opacity = 1, 
                color = "transparent", 
                group = "Distances", 
                popup = burger_labels, 
                highlight = highlightOptions(weight = 2.5, 
                                             color = "#666", 
                                             bringToFront = TRUE, 
                                             opacity = 1),
                popupOptions = popupOptions(autoPan = FALSE,
                                            closeOnClick = TRUE, 
                                            textOnly = T)
    ) %>% 
    addLegend(pal = pal, 
              values = (burger_distances$distance_km), 
              opacity = 0.8, 
              title = "Distance (Km)", 
              position= "bottomright"
    ) %>% 
    addLayersControl(overlayGroups = c("Burger locations", "Distances"),
                     options = layersControlOptions(collapsed = FALSE))
full_map

full_map2 <- leaflet() %>%
    addTiles() %>%
    addPolygons(
        data = burger_distances[[1]],
        fillColor = "#BDBDBD",
        fillOpacity = 0.7,
        weight = 0.4
    ) %>%
    addPolygons(
        data = burger_distances
    )

full_map2

ggplot(syd_map_sf) +
    geom_sf() +
    
    #Plot restaurants as points
    geom_point(data = sample_data,
               mapping = aes(x = burger.x,
                             y = burger.y,
                             color = relevant),
               size = 1.2) +
    
    theme_void() + 
    
    # Plot grid square we want to measure from. (Eg Bankstown airport)
    geom_sf(data = grid_wgs[1500,],
            aes(geometry = geometry),
            fill = "#196432",
            color = "#196432",
            size = 1
    ) +
    plot_annotation(
        title = "Sydney, with one random 2km<sup>2</sup> area highlighted",
        theme = theme(plot.title = element_markdown(size = 12))
    )


# ----

us = st_transform(
    st_read(
        here::here("data", "interim", "GCCSA_2016_AUST.shp"),
        quiet = TRUE,
    ), 
    crs = 4326
)

# ----
leaflet() +
  addTiles() %>% 
  addMarkers(data = burger_locations_wgs, 
             icon = ~burger_icon, 
             group = "Burger locations") %>% 
  addPolygons(map = syd_map_wgs)#, 
                 coords = c("lon", "lat"))
  addPolygons(data = burger_distances[[1]], 
              fillColor = pal(burger_distances$distance_km), 
              fillOpacity = 0.8, 
              weight = 0, # 0,
              opacity = 1, 
              color = "transparent", # "transparent",
              group = "Distances", 
              popup = burger_labels, 
              highlight = highlightOptions(weight = 2.5, 
                                           color = "#666", 
                                           bringToFront = TRUE, 
                                           opacity = 1),
              popupOptions = popupOptions(autoPan = FALSE,
                                          closeOnClick = TRUE, 
                                          textOnly = T)
  ) %>% 

mapbox