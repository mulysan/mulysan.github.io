
library(tidyverse)
library(httr2)
library(docstring)

gov_api <- "https://data.gov.il/api/3/action/datastore_search"
# Parameters (headers to call gov il api)
params <- list(
  resource_id = "c8b9f9c8-4612-4068-934f-d4acd2e3c06e",
  limit = 5,
  q = numeric(5)
)

check_number <- function(number_to_check){
  
  #' Check if a number is a handicap plate
  #' 
  #' Check weather a license plate number is a handicap plate or not. Get the date when the authorization was given.
  #' @param number_to_check a numeric value. the license plate number. has to be at least 6 digits long.
  #' @returns A numeric matrix with the license plate number and the date when the authorization was given. NULL if the plate is not a handicap plate.
  #' @note To convert the integer to a valid date format use `as.Date(as.character(DATE), format = "%Y%m%d")`.
  #' @examples
  #' check_number(1110111) # This number is a handicap plate
  
  # err handling
  if(!is.numeric(number_to_check) || nchar(number_to_check) < 6) return(NULL) # if longer, function runs and will return NULL. 
  # verbose msg
  cat('checking number:', number_to_check, '\n')
  
  # functionality
  params$q <- number_to_check
  full_details <-  request(gov_api) %>%
    req_url_query(!!!params) %>% #unpack list as named argument, like *tupple() in python
    req_perform() |> resp_body_json() %>% .[[3]] 
 
 if(is_empty(full_details$records)) return(NULL) # if plate is not handicap, return NULL
 
 
 date_as_int <- full_details[['records']][[1]][[3]] # else, record date
 # date <- as.Date(as.character(date_as_int), format = "%Y%m%d") -- do it afterwards on a tibble after binding rows
return(
  matrix(
    c(number_to_check, date_as_int),ncol = 2
  )
) 
}



yes <- 1110111

s <- Sys.time()
check_number(yes)
Sys.time() - s

set.seed(1110111)
some_plates <- sample(111111:99999999, 5)

df <-  lapply(c(1110111,some_plates), check_number)
df %>% do.call(rbind, .) |> as_tibble() |> rename(plate=1,date_issued=2) |> 
  mutate(date_issued = as.Date(as.character(date_issued), format = "%Y%m%d"))
