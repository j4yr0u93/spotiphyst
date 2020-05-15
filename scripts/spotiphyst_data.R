#scripting space for data pulling/scraping

library(tidyr)
library(dplyr)
library(readr)
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
  
  #create distinct search terms to feed to spotify API
  last_fm_terms <- last_fm_data %>%
    select(-c("date", "posix_date_time", "dow", "date_unix")) %>%
    distinct() %>%
    rowwise()
  #return df with track and artist ids for/from spotify API
  last_fm_ids <- mutate(last_fm_terms, ids_bundled = track_id_lookup(track, artist, album, spotify_key, spotify_secret)) %>%
    mutate(id = str_extract(string = ids_bundled, pattern = "[:alnum:]{22}"), artist_id = str_extract(string = ids_bundled, pattern = "[:alnum:]{22}$")) %>%
    select(-c(ids_bundled)) %>%
    filter(!is.na(id), !is.na(artist_id))

  #retrieve audio feature information
  audio_features <- add_spotify_features(spotify_key, spotify_secret, last_fm_ids)
  #
  track_features_ids <- full_join(last_fm_ids, audio_features)
  
  feature_data <- full_join(last_fm_data, track_features_ids)
  
  artist_genres <- get_artist_genres(spotify_key, spotify_secret, last_fm_ids)
  
  full_data <- full_join(feature_data, artist_genres)
  
  return(full_data)
  
}

#lookup track id with track, artist, and album name
track_id_lookup <- function(track, artist, album, spotify_key, spotify_secret){
  
  #get spotify search results
  result <- spotifyr::search_spotify(paste(track, artist, album, sep = " "), type = "track", limit = 1, authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret))
  #create df for non-results
  id <- c(NA)
  artists <- list(c(NA, NA))
  no_ret <- data.frame(id, artists)
  #see if result is empty and create df with id == NA
  if(nrow(result) == 0){
    result = no_ret
  }
  #set track_id to result id val
  ids_bundled <- paste(result$id[1], str_extract(unlist(result$artists)[2], pattern = "[:alnum:]{22}"), sep = "__")
  return(ids_bundled)
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

#get genres for each artist
get_artist_genres <- function(spotify_key, spotify_secret, last_fm_ids){
  
  last_fm_artist_ids <- last_fm_ids %>%
    select(-c("track", "id", "album")) %>%
    distinct()
  
  subsetted_id_dfs <- list()
  id_df <- last_fm_artist_ids %>% select(artist_id)
  div<-seq(50,(nrow(id_df)+49),50)
  for(i in 1:length(div)) {
    subsetted_id_dfs[[i]]<-id_df[(50*(i-1)+1):div[i],1]
  }
  #remove NA values from remainder list
  subsetted_id_dfs <- lapply(subsetted_id_dfs, function(x) x[!is.na(x)])
  
  #create empty df for rbind
  artist_genres <- colnames(data.frame()) %>% c("id", "genres")
  #loop for getting subset info
  for (i in 1:length(subsetted_id_dfs)) {
    genres_temp <- spotifyr::get_artists(unlist(subsetted_id_dfs[i]), authorization = spotifyr::get_spotify_access_token(spotify_key, spotify_secret)) %>%
      select(c("id", "genres"))
    artist_genres <- rbind(artist_genres, genres_temp)
  }
  
  artist_genres <- artist_genres %>% rename(artist_id = id)
  
  return(artist_genres)
  
}

###above is completed on first pass

###

all_data <- get_user_data("j4yr0u93")

##testing functions and misc

str(all_data)
  
write.csv(all_data, file = "all_music_data.csv")

j4yr0u93_music_data_2019 <- all_data %>% filter(!str_detect(genres, 'asmr')) %>% mutate(Month = month(date), Year = year(date), Time = hour(posix_date_time)) %>% filter(Year == 2019) %>%
  select(-c("date_unix", "date", "posix_date_time", "id", "artist_id", "type", "uri", "track_href", "analysis_url"))

saveRDS(j4yr0u93_music_data_2019, file="j4yr0u93_music_data_2019.Rda", version = 3, compress = FALSE)

genres_list <- list()
for (i in 1:nrow(j4yr0u93_music_data_2019)){
  
  genres_list <- append(genres_list, unlist(j4yr0u93_music_data_2019$genres[i]))
  
}

top_genres <- names(sort(summary(as.factor(c(unlist(genres_list)))), decreasing=T)[1:35])

unique(genres_list)

`Genre Options` <- list("pop", "hip hop", "rock", "rap", "alternative metal", "alternative r&b", "alternative rock", "neo-psychedelic", "indie rock", "classical")

test <- load("./data/j4yr0u93_music_data_2019.RData")

j4yr0u93_music_data_2019 <- j4yr0u93_music_data_2019 %>% select(-c(Year))


str(j4yr0u93_music_data_2019)

test <- j4yr0u93_music_data_2019 %>% filter(str_detect(genres, "\n"))

load("./data/j4yr0u93_music_data_2019.RData")


j4yr0u93_music_data_2019 <- j4yr0u93_music_data_2019 %>%


