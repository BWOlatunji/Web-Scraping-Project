
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
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

  states_postalcode_url_tbl <- all_state_names_tbl |>
    left_join(state_urls_tbl, by = join_by(state_id)) |>
    mutate(complete_state_postalcodes_url = str_glue("https://www.geonames.org{state_ext_url}"))
  
  return(states_postalcode_url_tbl)
  
}

states_postalcode_url_tbl <- get_states_url(states_in_usa_url)

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
plan("multicore")
states_postalcodes_tbl <- states_postalcode_url_tbl |>
  mutate(postalcode = future_map(complete_state_postalcodes_url, get_postal_codes)) |>
  unnest(postalcode)


# Now, we generate links that will enable us to get results of all obgyn within each postal code
states_postalcodes_tbl <- states_postalcodes_tbl |> 
  select(state_id,state_name,Place,PostalCode,Country,Admin2) |> 
  mutate(
        postalcodes_search_url = str_glue("https://www.acog.org/womens-health/find-an-ob-gyn/results?firstname=&lastname=&address={PostalCode}&searchradius=10")
    )


# CREATE DIRECTORY AND SAVE WEB SCRAPPED RESULTS CONTAINING STATES AND THEIR POSTAL CODES
write_rds(states_postalcodes_tbl, "data/states_postalcodes_tbl.rds")



