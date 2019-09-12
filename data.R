#OurHomesOurVote2020\

library(tidyverse)
library(rtweet)
library(RCurl)
library(RJSONIO)
library(plyr)
library(tidyverse)
library(leaflet)

url <- function(address, return.call = "json", sensor = "false" , key) {
  
  root <- "https://maps.google.com/maps/api/geocode/"
  
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, "&key=" , key , sep = "")
  
  return(URLencode(u))
}

# This function will return the lat, long, location_type , and formatted address

geocode <- function(address, api.key , verbose=FALSE) {
  
  if(verbose) cat(address,"\n")
  
  u <- url(address , key = api.key)
  
  doc <- getURL(u)
  
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status=="OK") {
    
    lat <- x$results[[1]]$geometry$location$lat
    
    lng <- x$results[[1]]$geometry$location$lng
    
    location_type  <- x$results[[1]]$geometry$location_type
    
    formatted_address  <- x$results[[1]]$formatted_address
    
    return(c(lat, lng, location_type, formatted_address))
    
    Sys.sleep(0.5)
    
  } else {
    
    return(c(NA,NA,NA, NA))
    
  }
}

# This function will return a json response from the api call

geocall <- function(address, api.key , verbose=FALSE) {
  
  if(verbose) cat(address,"\n")
  
  u <- url(address , key = api.key)
  
  doc <- getURL(u)
  
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status=="OK") {
    
    return(x)
    
    Sys.sleep(0.5)
    
  } else {
    
    return(x$error_message)
    
  }
}

get_embed <- function(screen_name, status_id){
  
  api_result <- httr::GET(paste0(
    "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2F",
    screen_name,
    "%2Fstatus%2F",
    status_id))
  
  api_content <- httr::content(api_result)
  html_content <- api_content[["html"]]
  
  return(html_content)
}


split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[,col])

hashtag <- "#OurHomesOurVotes2020"

stream <- search_tweets(q = hashtag , include_rts = T , n = 1000 , retryonratelimit = T) %>% 
  lat_lng()


locations <- stream %>% 
  filter(is.na(lat) | is.na(lng)) %>% 
  filter(!is.na(location) | location != "") %>% 
  pull(location) %>% unique()

#You need a google maps API key to run this function
geocoded  <- ldply(locations, .progress = "text" , function(x) geocode(x , api.key = key))
names(geocoded) <- c("lat","lng","location_type", "formatted")

geocoded <- bind_cols(location = locations , geocoded)

stream.geo <- stream %>% 
  filter(is.na(lat) | is.na(lng)) %>% 
  filter(!is.na(location) | location != "") %>% 
  select(user_id , status_id , created_at , name , screen_name , profile_image_url ,
         text , hashtags , favorite_count , retweet_count , quote_count ,
         reply_count , place_name , place_full_name , place_type , place_url , location ,
         geo_coords , coords_coords , bbox_coords ,
         country) %>% 
  left_join(geocoded %>% select(location , lat , lng), by = "location")

stream.geo <- stream %>% filter(!is.na(lat) | !is.na(lng)) %>% 
  select(user_id , status_id , created_at , name , screen_name , profile_image_url ,
         text , hashtags , favorite_count , retweet_count , quote_count ,
         reply_count , place_name , place_full_name , place_type , place_url , location ,
         geo_coords , coords_coords , bbox_coords ,
         country , lat , lng) %>% 
  bind_rows(stream.geo %>% 
              mutate(lat = as.numeric(as.character(lat)) , 
                     lng = as.numeric(as.character(lng))
              )
  )

t.storm  <- stream.geo %>% mutate(created_at = lubridate::as_datetime(created_at)) %>% 
  filter(created_at >= lubridate::as_datetime("2019-09-11 19:00:00 UTC") & 
           created_at <= lubridate::as_datetime("2019-09-11 20:00:00 UTC")) %>% 
  mutate(minute = lubridate::minute(created_at)) %>% 
  arrange(lubridate::minute(created_at)) 

embedded <- t.storm %>% 
  group_by(status_id) %>% 
  dplyr::transmute(embedded = get_embed(screen_name = screen_name , status_id = status_id))

t.storm <- t.storm %>% 
 left_join(embedded , by = "status_id")

