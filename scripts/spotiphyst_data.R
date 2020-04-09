#testing space

library(lubridate)
library(knitr)
library(stringr)

#function to get user music data and clean it
get_user_data <- function(userID){

  #get keys from TOML
API_keys <- RcppTOML::parseToml("API_keys.toml")

  #assign key
lastfm_key <- API_keys$keys$lastfm_API_key

  #pull lastfm timestamps and plaintext info
last_fm_data <- last_fm_pull(lastfm_key, userID)

  #assign keys
spotify_key <- API_keys$keys$spotify_API_key
spotify_secret <- API_keys$keys$spotify_secret

  #garbage
#user_data <- spotify_features(spotify_key, spotify_secret, last_fm_data)

}

#function to pull last_fm_data
last_fm_pull <- function(lastfm_key, userID){
  
  last_fm_raw <- scrobbler::download_scrobbles(username = userID, api_key = lastfm_key)
  
  last_fm_formatted <- last_fm_raw %>%
    mutate(posix_date_time = as.POSIXct(as.numeric(date_unix), origin = '1970-01-01', tz = 'ET')) %>%
    mutate(date = as.Date(posix_date_time)) %>%
    mutate(dow = factor(weekdays(date), ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    dplyr::select(-c("song_mbid", "artist_mbid", "album_mbid"))
  
  return(last_fm_formatted)
  
}

#get audio feature table and then do lookups
spotify_features <- function(spotify_key, spotify_secret, last_fm_data){
 
  artist_table <- audio_feature_lookup(spotify_key, spotify_secret, last_fm_data)
  
  
}

audio_feature_lookup <- function(spotify_key, spotify_secret, last_fm_data){}



my_data <- get_user_info("j4yr0u93")






