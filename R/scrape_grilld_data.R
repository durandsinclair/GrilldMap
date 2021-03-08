# SCRAPE GRILLD DATA

# Scrapes the data from the Grilld website and turns them into a tibble.

# The website is at: https://www.grilld.com.au/restaurants/new-south-wales/sydney
# The addresses themselves are in the class called "restaurant-list-item-address"

# 0) PREPARE ENVIRONMENT --------------------------------------------------
    
    # Clear global environment
    rm(list = ls() )
    dev.off()
    gc()
    cat("\014")
    

    # Load libraries
    library(tidyverse)
    library(here)
    library(rvest)
    library(ggmap)


# 1) SCRAPE DATA ----------------------------------------------------------

scrape_one_page <- function(url, city, geocode_this_page = FALSE,
                            add_one = FALSE) {
    # Get whole webpage
    webpage <- read_html(url)
    
    # Scrape the class from the webpage
    all_addresses <- html_nodes(webpage, ".restaurant-list__item__address") %>% 
        html_text() %>%
        str_replace_all("\n", " ") %>%
        str_replace("\\(", "") %>%
        str_replace_all("\\)", "") #%>%
    
    names <- html_nodes(webpage, ".restaurant-list__item__heading") %>%
        html_text() 

    # Turn into a tibble
    raw_tbl <- tibble(name = names, a_address = all_addresses, ) %>%
        separate(a_address, c("raw_address", "phone"), " 02 ") %>%
        mutate( raw_address = str_trim(raw_address)) %>%
        mutate( phone = str_trim(phone)) %>%
        mutate( city = city)
    
    
    if (geocode_this_page == TRUE) {
        raw_tbl <- mutate_geocode(
            raw_tbl,
            location = name,
            output = "latlona"
        )
        # Add one?
        if (add_one == TRUE) {
            extra <- tibble(
                name = "Random Square",
                raw_address = "Kanangra Walls Lookout, Kanangra NSW 2787",
                phone = "",
                city = "Sydney",
                lon = 150.6975,
                lat = -33.9757,
                address = "Kanangra Walls Lookout, Kanangra NSW 2787"
            )
            raw_tbl <- raw_tbl %>%
                rbind(extra)
        } 
        
    
    } else {
        # Add one?
        if (add_one == TRUE) {
            
            extra = tibble(
                name = "Random Square",
                raw_address = "Kanangra Walls Lookout, Kanangra NSW 2787",
                phone = "",
                city = "Sydney"
            )
            raw_tbl <- raw_tbl %>%
                rbind(extra)
        } 
    }
        
    # Return the table
    raw_tbl
}

tidy_scraped_page <- function(raw_tbl){
    tidy_tbl <- raw_tbl %>%
        select(name,
               address = raw_address,
               phone,
               city, 
               lon, 
               lat
        ) %>%
        
        mutate(address2 = address) %>%
        
        separate(
            col = address2,
            into = c("address_line_0","address_line_1", "address_line_2"),
            sep = ",",
            extra = "merge",
            fill = "left",
        ) %>%
        mutate(address_line_2 = str_trim(address_line_2)) %>%
        mutate(address_line_0 = replace_na(address_line_0, "")) %>%
        mutate(address_line_1 = if_else(
            address_line_0 == "",
            address_line_1,
            paste0(address_line_0, ",", address_line_1)
        )) %>%
        mutate(address_line_2 = str_to_title(address_line_2)) %>%
        select(-address_line_0) %>%
        select(-address) %>%
        rename(address1 = address_line_1) %>%
        rename(address2 = address_line_2)
    tidy_tbl
}

scrape_and_tidy <- function(url, city, add_one = FALSE) {
    raw <- scrape_one_page(url, city, TRUE, add_one)
    tidy <- tidy_scraped_page(raw)
}
# 2) SCRAPE SYDNEY DATA ---------------------------------------------------

url <- "https://www.grilld.com.au/restaurants/new-south-wales/sydney"
city = "Sydney"
geocode_this_page = TRUE
add_one = TRUE

raw <- scrape_and_tidy(url, city)
raw

raw2 <- scrape_and_tidy(url, city, TRUE)
raw2

write_csv(x = raw2, path = here::here("data", "processed", "locations_plus_one.csv"))

location_tbl <- raw %>%
    mutate(id = 1:nrow(.), .before = name) %>%
    select(id, name, address1, address2, phone, lon, lat)
location_tbl

write_csv(x = location_tbl, path = here::here("data", "processed", "locations_tbl.csv"))

locations <- raw %>%
    mutate(id = 1:nrow(.), .before = name)
locations

write_csv(x = locations, path = here::here("data", "processed", "locations.csv"))

