#testing space

library(tidyr)

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
  
  last_fm_data <- last_fm_raw %>%
    rename(track = song_title) %>%
    mutate(posix_date_time = as.POSIXct(as.numeric(date_unix), origin = '1970-01-01', tz = 'ET')) %>%
    mutate(date = as.Date(posix_date_time)) %>%
    mutate(dow = factor(weekdays(date), ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    select(-c("song_mbid", "artist_mbid", "album_mbid"))
  
}

###

#get audio features and other relevant info
spotify_features <- function(spotify_key, spotify_secret, last_fm_data){
  
  last_fm_data_id <- last_fm_data %>%
    rowwise() %>%
    mutate(id = track_id_lookup(track, artist, album, spotify_key, spotify_secret))
  
#  user_data_features <- add_spotify_features(spotify_key, spotify_secret, last_fm_data_id)
  
}

#lookup track id with track, artist, and album name
track_id_lookup <- function(track, artist, album, spotify_key, spotify_secret){
  
  frame <- spotifyr::search_spotify(paste(track, artist, album, sep = " "), type = "track", authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret))
  
  id <- frame$id[1]
  
  return(id)
}

#iterate through ID an join traack feature with main tbl
add_spotify_features <- function(spotify_key, spotify_secret, last_fm_data_id){
  
  for (id in last_fm_data_id) {
    features <- spotifyr::get_track_audio_features(id = id, authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret))
    full_join(last_fm_data_id, features)
  }
  
}

###

my_data <- get_user_data("j4yr0u93")

test2 <- spotify_features(spotify_key, spotify_secret, last_fm_data)

