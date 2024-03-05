library(tidyverse)
library(lubridate)
library(magrittr)
library(ggplot2)
library(rvest)
library(weathermetrics)
library(stringr)


###tibble with full list of ghcnd stations in the world 
all_stations <- read_table('Data Science Practice/Weather/ghcnd-stations.txt', col_names = c('stationid', 'latitude', 'longitude', 'elevation', 'name', 'X6', 'X7', 'X8', 'X9'))
all_stations[all_stations %>% is.na] <- '' #changing NAs to blanks

all_stations$name <- paste(all_stations$name, all_stations$X6, all_stations$X7, all_stations$X8, all_stations$X9, sep = " ") #combining name elements into one column
all_stations <- all_stations[,c('stationid', 'latitude', 'longitude', 'elevation', 'name')]

all_stations[(all_stations$name %>% tolower) %>% grep('ohare', .),]$stationid #sanity check to know I can search for specific IDs by basic name
all_stations[all_stations$stationid %>% grep('USW', .),] #just checking

all_stations %>% write_csv(file = 'Data Science Practice/Weather/clean_ghcnd-stations.csv')
###

###creating a list of the 50 biggest cities in the US. formatted as "City, ST"
big_citiesdf <- (read_html('https://www.infoplease.com/us/cities/top-50-cities-us-population-and-rank') %>% html_nodes('table') %>% html_table(header = TRUE))[[1]]
big_citiesdf %<>% separate(., City, into = c('City', 'State'), sep = ', ')
big_citiesdf$State <- state.abb[match(big_citiesdf$State, state.name)]
big_citiesdf[big_citiesdf == 'New York City'] <- 'New York'
big_citiesdf$State[big_citiesdf$City == 'Washington'] <- 'DC'

big50_cities <- paste0(big_citiesdf$City, ', ', big_citiesdf$State) #final list
big50_cities %>% saveRDS(file = 'Data Science Practice/Weather/big_50_US_cities.RData') #saved in case that link ever becomes deprecated
###

###wban stations have better naming conventions than ghcnd. cleaning first
wban_stations
wban_stations <- read.table('Data Science Practice/Weather/wbanmasterlist.psv', sep = '|', header = TRUE)
wban_stations <- wban_stations[, !names(wban_stations) %in% 'COMMENTS']
wban_stations$STATION_NAME[wban_stations$STATION_NAME == 'COLORADO SPINGS'] <- 'COLORADO SPRINGS'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'DFW'] <- 'DALLAS'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'ORF'] <- 'VIRGINIA BEACH'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'FTG'] <- 'AURORA'

wban_stations %<>% unite(STATION_NAME, STATION_NAME, STATE_PROVINCE, sep = ', ', remove = TRUE)

#filter for stations located in 50 biggest cities
big50_stations <- wban_stations[wban_stations$STATION_NAME %in% (big50_cities %>% toupper()), ] #%>% group_by(STATION_NAME) %>% summarise(count = n()) %>% print(n = 50)


#function to build out the full ghcn id from the wban id. just missing trailing zeroes
make_full_id <- function(id){ 
  id %<>% toString()
  len <- id %>% str_length()
  num_zeroes <- 8 - len
  
  zeroes <- replicate(num_zeroes, '0') %>% paste(collapse = '')
  
  full_id <- paste0('USW', zeroes, id)
  return(full_id)
}


big50_stations %<>% mutate(ghcn_id = sapply(big50_stations$WBAN_ID, make_full_id), .after = WBAN_ID) #adding ghcn ids to big50_stations

big50_stations[big50_stations$ghcn_id %in% all_stations$stationid,] %>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n = 50) #checking number of stations for each city

final50_stations <- big50_stations[big50_stations$ghcn_id %in% all_stations$stationid & big50_stations$CALL_SIGN != '',] #%>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n = 50)

final50_stations %>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n=50)
final50_stations[final50_stations$STATION_NAME == 'CHICAGO, IL',]

final50_stations %>% write_csv(file = 'Data Science Practice/Weather/final50_stations.csv')

###

###function that gathers noaa data from stations' ghcn ID, from a specified date range (yyyy-mm-dd)
weather_scrape <- function(stations, startdate, enddate){
  weather_url <- paste0('https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&stations=', stations,
                        '&startDate=', startdate, '&endDate=', enddate, '&boundingBox=90,-180,-90,180')
  weather_data <- read_csv(weather_url)
  return(weather_data)
}