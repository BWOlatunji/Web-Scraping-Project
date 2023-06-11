# ACOG Data
# search urls

acog_search_url_10 <- "https://www.acog.org/womens-health/find-an-ob-gyn/results?firstname=&lastname=&address=77469&searchradius=10"
acog_search_url_100 <- "https://www.acog.org/womens-health/find-an-ob-gyn/results?firstname=&lastname=&address=77469&searchradius=100"


acog_html_10 <- read_html(acog_url_10)
acog_html_100 <- read_html(acog_url_100)

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

# TEST WITH ONE POSTAL CODE
get_all_profile_info(acog_search_url_10) |> view()


# PROCESSED IN PARALLEL with furrr (5 minutes)
plan("multicore")
profile_tbl_1 <- states_postalcodes_tbl[1:100,] |>
  mutate(obgyn_profile = future_map(postalcodes_search_url, get_all_profile_info)) |>
  unnest(obgyn_profile)

profile_tbl_2 <- states_postalcodes_tbl[101:150,] |>
  mutate(obgyn_profile = future_map(postalcodes_search_url, get_all_profile_info)) |>
  unnest(obgyn_profile)


profile_tbl_3 <- states_postalcodes_tbl[151:200,] |>
  mutate(obgyn_profile = future_map(postalcodes_search_url, get_all_profile_info)) |>
  unnest(obgyn_profile)

profile_alaska <- 

# getting the insurance info from individual ob-gyn web page

insurance_tbl <- profile_tbl_1 |> 
  mutate(
        individual_url = str_glue("https://www.acog.org{profile_url}")
    ) 

insurance_tbl_2 <- profile_tbl_2 |> 
  mutate(
        individual_url = str_glue("https://www.acog.org{profile_url}")
    ) 

insurance_tbl_3 <- profile_tbl_3 |> 
  mutate(
        individual_url = str_glue("https://www.acog.org{profile_url}")
    ) 

# test
test_obgyn_ind <- "https://www.acog.org/womens-health/find-an-ob-gyn/physician?id=4f42a862-45c6-483e-86a7-712895663dca"

test_obgyn_ind |> 
  read_html() |> 
  html_element("address") |>
  html_text2() 
# output
# [1] "\r ANCHORAGE WOMEN'S CLINIC\r\n\r 3260 Providence Dr Ste 425 \r Anchorage, AK 99508-4603\r United States\n\r Phone: \r \r (907) 561-7111\r\n\r Fax: \r \r (907) 770-7891\n\r Website: \r \r anchoragewomensclinic.com\r\n\r Email: \r \r [email protected]\r\n\r \r Accepts Medicaid\n\r \r Accepts Medicare\nGet Directions\n"


contact_info <- test_obgyn_ind |> 
  read_html() |> 
  html_element("address") |>
  html_text2() |> enframe(name = "position", value = "ind_address") |> 
  separate(ind_address, c("Contact Info", "Insurance 1", "Insurance 2"), sep = "\\sAccepts",extra = "merge") |> 
  mutate(`Insurance 2` = `Insurance 2` |> str_remove_all("Get Directions"))


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

get_insurance_info(test_obgyn_ind) |> View()

# PROCESSED IN PARALLEL with furrr (5 minutes)
plan("multicore")
obgyn_insurance_tbl_50 <- insurance_tbl[1:50,] |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 

obgyn_insurance_tbl_100 <- insurance_tbl[51:100,] |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 

obgyn_insurance_tbl_110 <- insurance_tbl[101:110,] |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 

# second batch

obgyn_insurance_tbl_101_150 <- insurance_tbl_2 |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 

obgyn_insurance_tbl_151_200 <- insurance_tbl_3 |>
  mutate(obgyn_insurance = future_map(individual_url, get_insurance_info)) |>
  unnest(obgyn_insurance) 


obgyn_insurance_alaska <- obgyn_insurance_tbl_50 |> 
  bind_rows(obgyn_insurance_tbl_100) |> 
  bind_rows(obgyn_insurance_tbl_110) |> 
  bind_rows(obgyn_insurance_tbl_101_150) |> 
  bind_rows(obgyn_insurance_tbl_151_200) |> 
  select(-c(ins_id, `Contact Info`))


write_csv(obgyn_insurance_alaska,file = "data/obgyn_alaska.csv")
