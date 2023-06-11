
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(fs)        # Working with File System
library(xopen)     # Quickly opening URLs

# url containing list of all states in USA

states_in_usa_url <- "https://www.geonames.org/postal-codes/postal-codes-us.html"

get_states_url <- function(all_states_url) {
  # get all state names from the web page
  all_state_names_tbl <- all_states_url |>
    read_html() |>
    html_element("div") |>
    html_elements("a") |>
    html_text2() |>
    enframe(name = "state_id", value = "state_name")
  # get the extended url for each state that list all the postal codes for the state
  state_urls_tbl <- all_states_url |>
    read_html() |>
    html_element("div") |>
    html_elements("a") |>
    html_attr("href") |>
    enframe(name = "state_id", value = "state_ext_url")
  
  # using this link as an example: "https://www.geonames.org/postal-codes/US/AK/alaska.html",
  # let's generate the complete url for the web page that has the list of the postal codes for each state
  # We achieve this by joining the main web address to the extended url for the state
  states_postalcode_url_tbl <- all_state_names_tbl |>
    left_join(state_urls_tbl, by = join_by(state_id)) |>
    mutate(complete_state_postalcodes_url = str_glue("https://www.geonames.org{state_ext_url}"))
  
  return(states_postalcode_url_tbl)
  
}


# tibble containing web page links for each states where we can scrape all their postal codes
states_postalcode_url_tbl <- get_states_url(states_in_usa_url)


# getting postal codes within each state
# class=restable

# Make Function to Get all postal codes within each state ----
get_postal_codes <- function(pc_url) {

    state_pc_tbl <- pc_url |> 
      read_html() |>
      html_element(".restable") |>
      html_table() |> 
      set_names(c("pc_id", "Place", "PostalCode", "Country", "Admin1", "Admin2", "Admin3")) |> 
      filter(!is.na(pc_id)) |> 
      select(-Admin3)
    
    return(state_pc_tbl)
}

# Scrape all postal codes in each state ----
# - RUNNING THE CODE IN PARALLEL IT TAKES ABOUT 5 MINUTES, 
# BUT COULD RUN INTO ISSUES WITH TIMEOUTS

# PROCESSED IN PARALLEL with furrr (5 minutes)
plan("multicore")
states_postalcodes_tbl <- states_postalcode_url_tbl |>
  mutate(postalcode = future_map(complete_state_postalcodes_url, get_postal_codes)) |>
  unnest(postalcode)


# Now, we generate links that will enable us to get results of all obgyn within each postal code
states_postalcodes_tbl <- states_postalcodes_tbl |> 
  select(state_id,state_name,Place,PostalCode,Country,Admin2) |> 
  # add postal code to generate unique search url by postal code
  mutate(
        postalcodes_search_url = str_glue("https://www.acog.org/womens-health/find-an-ob-gyn/results?firstname=&lastname=&address={PostalCode}&searchradius=10")
    )


# CREATE DIRECTORY AND SAVE WEB SCRAPPED RESULTS CONTAINING STATES AND THEIR POSTAL CODES
fs::dir_create("data")
write_rds(states_postalcodes_tbl, "data/states_postalcodes_tbl.rds")



