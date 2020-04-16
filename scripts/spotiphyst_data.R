#scripting space for data pulling/scraping

library(tidyr)
library(dplyr)
library(readr)

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

  user_data <- spotify_features(spotify_key, spotify_secret, last_fm_data)

  return(user_data)
  
}

#function to pull last_fm_data
last_fm_pull <- function(lastfm_key, userID){
  
  last_fm_raw <- scrobbler::download_scrobbles(username = userID, api_key = lastfm_key)
  
  last_fm_data <- last_fm_raw %>%
    rename(track = song_title) %>%
    mutate(posix_date_time = as.POSIXct(as.numeric(date_unix), origin = '1970-01-01', tz = 'EST')) %>%
    mutate(date = as.Date(posix_date_time)) %>%
    mutate(dow = factor(weekdays(date), ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    select(-c("song_mbid", "artist_mbid", "album_mbid"))
  
}

###

#get audio features and other relevant info
spotify_features <- function(spotify_key, spotify_secret, last_fm_data){
  
  last_fm_terms <- last_fm_data %>%
    select(-c("date", "posix_date_time", "dow", "date_unix")) %>%
    distinct() %>%
    rowwise()

  last_fm_ids <- mutate(last_fm_terms, id = track_id_lookup(track, artist, album, spotify_key, spotify_secret)) %>%
    filter(!is.na(id))

  audio_features <- add_spotify_features(spotify_key, spotify_secret, last_fm_ids)
  
  track_features_ids <- full_join(last_fm_ids, audio_features)
  
  full_data <- full_join(last_fm_data, track_features_ids)
  
  return(full_data)
  
}

#lookup track id with track, artist, and album name
track_id_lookup <- function(track, artist, album, spotify_key, spotify_secret){
  
  #get spotify search results
  result <- spotifyr::search_spotify(paste(track, artist, album, sep = " "), type = "track", limit = 1, authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret))
  #create df for non-results
  id <- c(NA)
  no_ret <- data.frame(id)
  #see if result is empty and create df with id == NA
  if(nrow(result) == 0){
    result = no_ret
  }
  #set track_id to result id val
  track_id <- result$id[1]  
  return(track_id)
}

#iterate through ID and join track feature with main tbl
add_spotify_features <- function(spotify_key, spotify_secret, last_fm_ids){
  
  #create list of id lists per 100 ids or less (remainder list)
  subsetted_id_dfs <- list()
  id_df <- last_fm_ids %>% select(id)
  div<-seq(100,(nrow(id_df)+99),100)
  for(i in 1:length(div)) {
    subsetted_id_dfs[[i]]<-id_df[(100*(i-1)+1):div[i],1]
  }
  #remove NA values from remainder list
  subsetted_id_dfs <- lapply(subsetted_id_dfs, function(x) x[!is.na(x)])
  
  #feature call
  audio_features <- colnames(data.frame()) %>%
    c("acousticness", "analysis_url", "danceability", "duration_ms", "energy", "id", "instrumentalness", "key",
      "liveness", "loudness", "mode", "speechiness", "tempo", "time_signature", "track_href", "type", "uri", "valence")
  for (i in 1:length(subsetted_id_dfs)) {
    features_temp <- spotifyr::get_track_audio_features(unlist(subsetted_id_dfs[i]), authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret))
    audio_features <- rbind(audio_features, features_temp)
  }

  return(audio_features)

}

###above is completed on first pass

###

all_data <- get_user_data("j4yr0u93")

##testing functions and misc

str(all_data)
  
write.csv(all_data, file = "all_music_data.csv")