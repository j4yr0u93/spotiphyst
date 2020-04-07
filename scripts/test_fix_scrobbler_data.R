#testing space

library(tidyverse)
library(stringr)
library(scrobbler)
library(spotifyr)

last_fm_raw <- download_scrobbles(username = "j4yr0u93", api_key = "7cbb82a48708ebc982d41a8127eccaec")

my_data <- last_fm_raw %>%
  mutate(posix_date_time = as.POSIXct(as.numeric(date_unix), origin = '1970-01-01', tz = 'ET')) %>%
  mutate(date = as.Date(date_time)) %>%
  mutate(doy = weekdays(date)) %>%
  factor(doy, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  

unique(my_data$date_time)

str(my_data)
