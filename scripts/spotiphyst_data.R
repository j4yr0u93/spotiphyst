#testing space

library(tidyverse)
library(lubridate)
library(knitr)
library(stringr)
library(spotifyr)

get_user_info <- function(userID){
  
API_keys <- RcppTOML::parseToml("API_keys.toml")

}


last_fm_raw <- scrobbler::download_scrobbles(username = "j4yr0u93", api_key = "7cbb82a48708ebc982d41a8127eccaec")

last_fm_formatted <- last_fm_raw %>%
  mutate(posix_date_time = as.POSIXct(as.numeric(date_unix), origin = '1970-01-01', tz = 'ET')) %>%
  mutate(date = as.Date(posix_date_time)) %>%
  mutate(dow = factor(weekdays(date), ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
  

unique(my_data$date_time)

str(my_data)
