# ACOG Data
library(tidyverse) # Main Package - Loads dplyr, purrr
library(rvest)     # HTML Web Scraping
library(furrr)     # Parallel Processing using purrr (iteration)
library(xopen)     # Quickly opening URLs

# data
states_postalcodes_tbl <- read_rds("data/states_postalcodes_data_tbl.rds")

# CREATE FUCNTION TO GET ALL PROFILE INFORMATION ON THE SEARCH RESULT LIST
get_all_profile_info <- function(st_pc_url) {
  
  # get addresses
  acog_addresses <- st_pc_url |>
    read_html() |>
    html_element("#listingTable") |>
    html_elements("li") |>
    html_element("address") |>
    html_text2() |> enframe(name = "position", value = "address") |>
    mutate(address = address |>
             str_replace_all(pattern = "Get Directions", replacement = "")) |> 
    mutate(address = address |> str_remove_all(pattern = "[\\r\\n+]")) |> 
    separate(address, c("address", "Phone Number"),sep = "Phone: ",extra = "merge", fill = "right")
   
  # get profile names and title
  acog_names_title <- st_pc_url |>
    read_html() |>
    html_element("#listingTable") |>
    html_elements("li") |>
    html_element("a") |>
    html_text2() |> enframe(name = "position", value = "names_title")|>
    separate(names_title, c("Names", "Title"),sep = ", ", extra = "merge", fill = "right")
  
  # get profile urls
  acog_profile_url <- st_pc_url |>
    read_html() |>
    html_element("#listingTable") |>
    html_elements("li") |>
    html_element("a") |>
    html_attr("href") |> enframe(name = "position", value = "profile_url")

  # combine all 3 tibbles  
  combined_tbl <- left_join(acog_addresses, acog_names_title, by=join_by(position)) |> 
  left_join(acog_profile_url, by=join_by(position))
  
  return(combined_tbl)
}


# PROCESSED IN PARALLEL with furrr (5 minutes)
plan("multicore")
profile_tbl <- states_postalcodes_tbl |>
  mutate(obgyn_profile = future_map(postalcodes_search_url, get_all_profile_info)) |>
  unnest(obgyn_profile)


# getting the insurance info from individual ob-gyn web page

profile_insurance_tbl <- profile_tbl |> 
  mutate(
        individual_url = str_glue("https://www.acog.org{profile_url}")
    ) 

# output
# [1] "\r ANCHORAGE WOMEN'S CLINIC\r\n\r 3260 Providence Dr Ste 425 \r Anchorage, AK 99508-4603\r United States\n\r Phone: \r \r (907) 561-7111\r\n\r Fax: \r \r (907) 770-7891\n\r Website: \r \r anchoragewomensclinic.com\r\n\r Email: \r \r [email protected]\r\n\r \r Accepts Medicaid\n\r \r Accepts Medicare\nGet Directions\n"


get_insurance_info <- function(obgyn_url){
  
  contact_info <- obgyn_url |> 
  read_html() |> 
  html_element("address") |>
  html_text2() |> enframe(name = "ins_id", value = "ind_address") |> 
  separate(ind_address, c("Contact Info", "Insurance 1", "Insurance 2"), sep = "\\sAccepts",extra = "merge", fill = "right") |> 
  mutate(across(3:4, ~ str_remove_all(.x, "Get Directions"))) |> 
  mutate(across(2:4, ~ str_replace_all(.x, pattern = "[\\r\\n+]",replacement = ""))) |> 
    mutate(across(2:4, str_trim))
  #mutate(`Contact Info` = `Contact Info` |> str_replace_all(pattern = "\\r\\n",replacement = ""))

  return(contact_info)
}

# PROCESSED IN PARALLEL with furrr (5 minutes)
# plan("multicore")
obgyn_profile_tbl <- profile_insurance_tbl |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 


write_csv(obgyn_profile_tbl,file = "data/obgyn_profile.csv")

write_rds(obgyn_profile_tbl, "data/obgyn_profile.rds")


