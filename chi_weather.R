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


#function to build out the full ghcn id from the wban id. justing missing trailing zeroes
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
  weather_url <- paste0('https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&stations=', chicago_station,
                        '&startDate=', startdate, '&endDate=', enddate, '&boundingBox=90,-180,-90,180')
  weather_data <- read_csv(weather_url)
  return(weather_data)
}


stations <- final50_stations$ghcn_id[1:50]
stations <- stations %>% paste0(collapse = ',')
chicago_station <- 'USW00094846'
startdate <- '1970-01-01'
enddate <- today()

final50_stations[final50_stations$CALL_SIGN == 'ORD',]

weather_data <- read_csv(weather_url) 

weather_data %<>% select(where(function(x) any(!is.na(x))))
weather_data %<>% select(-PGTM)
weather_data[,c('TAVG', 'TMAX', 'TMIN', 'ADPT', 'AWBT')] <- (weather_data[,c('TAVG', 'TMAX', 'TMIN', 'ADPT', 'AWBT')] / 10) %>% celsius.to.fahrenheit() #converting all temps from 10x C to F
weather_data[,c('AWND', 'WSF2', 'WSF5')] <- weather_data[,c('AWND', 'WSF2', 'WSF5')]*.2237 #windspeeds from kph to mph

winter_months <- c(12, 1, 2)


winter_data <- weather_data[weather_data$DATE %>% month() %in% c(1, 2, 3, 4, 10, 11, 12),]
winter_data %<>% mutate(WINTER = case_when(month(DATE) %in% c(10, 11, 12) ~ year(DATE),
                                           month(DATE) %in% c(1, 2, 3, 4) ~ year(DATE) - 1), .after = DATE)
winter_data %<>% mutate(DAY = yday(DATE), .before = DATE, CDATE = format(as.Date(winter_data$DATE), "%m-%d"))


ggplot(data = winter_data, mapping = aes(y = TMAX, x = WINTER_DAY)) + geom_line(aes(group = WINTER, color = factor(WINTER -WINTER %% 10)))
ggplot(data = winter_data, mapping = aes(y = TMIN, x = WINTER_DAY)) + geom_line(aes(group = WINTER, color = WINTER))


weather_data$DATE %>% day()


factor(winter_data$WINTER - winter_data$WINTER %% 10)

winter_data[order(match(month(winter_data$DATE), winter_months), winter_data$DATE), ]


winter_data %<>% mutate(WINTER_DAY = ave(winter_data$DAY, winter_data$WINTER, FUN = seq_along), WINTER_WEEK = ceiling(WINTER_DAY / 7), .after = DAY)
winter_data %>% group_by(DECADE = WINTER-WINTER %% 5) %>% summarize(mumax = mean(TMAX))
winter_data %>% group_by(DECADE = WINTER-WINTER %% 5, WINTER_DAY) %>% summarize(mumax = mean(TMAX))

winter_data %>% group_by(WINTER = WINTER, week = ceiling(WINTER_DAY / 7)) %>% summarize(min(DATE)) %>% print(n = 50)

winter_data
##############PLOTS##############


###Daily average temperatures (TMAX, TMIN) grouped by decade
ggplot(data = winter_data %>% group_by(DECADE = WINTER-WINTER %% 10, WINTER_DAY) %>% summarize(mumin = mean(TMIN)), mapping = aes(y = mumin, x = WINTER_DAY)) + geom_line(aes(group = DECADE, color = factor(DECADE)))
ggplot(data = winter_data %>% group_by(DECADE = WINTER-WINTER %% 10, WINTER_DAY) %>% summarize(mumax = mean(TMAX)), mapping = aes(y = mumax, x = WINTER_DAY)) + geom_line(aes(group = DECADE, color = factor(DECADE)))

###Weekly median temperatures (TMAX, TMIN) grouped by decade
ggplot(data = winter_data %>% group_by(DECADE = WINTER - WINTER %% 10, WINTER_WEEK) %>% summarize(mumin = median(TMIN)), mapping = aes(y = mumin, x = WINTER_WEEK)) + geom_line(aes(group = DECADE, color = DECADE))

###Median & avg temp across years for each month
med_avg_df <- winter_data %>% group_by(WINTER, MONTH = month(DATE)) %>% summarize(medmaxtemp = median(TMAX), medmintemp = median(TMIN), avgmaxtemp = mean(TMAX), avgmintemp = mean(TMIN)) #grouped df with median maxtemp for each month & year
long_monthly_temps <- med_avg_df %>% pivot_longer(-c(WINTER, MONTH), names_to = 'METRIC', values_to = 'TEMP')

###GREAT visualization of median monthly temps across time (WINTERS) grouped by each month
ggplot(data = transform(long_monthly_temps[long_monthly_temps$MONTH %in% c(12, 1, 2, 3) & long_monthly_temps$METRIC %in% c('medmaxtemp', 'medmintemp'),], MONTH = factor(MONTH, levels = c(12, 1, 2, 3))), mapping = aes(TEMP, x = WINTER, color = METRIC)) + geom_line() + facet_wrap(vars(MONTH), labeller = as_labeller(c('12' = 'December', '1' = 'January', '2' = 'February', '3' = 'March')), scales = "free") + 
  geom_hline(yintercept = 32, size = .4, alpha = .7, linetype = 'dashed') + #annotate("text", x = 1968, y = 30, label = "32°F", family = 'Optima') +
  geom_smooth(method = "lm", se = FALSE, linetype = 'dotted', size = .8, alpha = .6) + scale_y_continuous(limits = c(0, 68)) + labs(title = "Median Monthly Maximum & Minimum Temperatures", subtitle = ("(Note January, February, & March fall under the winter of the previous year\nE.G Jan 2001 is in the Winter of 2000)")) + ylab(label = 'Temperature (°F)') + xlab(label = 'Winter') + scale_colour_manual(values = c(medmaxtemp = '#ff0000', medmintemp = '#88c7dc'), name = '', labels = c('Median High Temperature', 'Median Low Temperature')) +
  theme_linedraw() + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5), text = element_text(size = 14, family = 'Optima'), panel.grid = element_line(size = .7, linetype = 'solid', color = '#000000'))



long_monthly_temps

dec_min_med_mod <- long_monthly_temps[long_monthly_temps$MONTH == 12 & long_monthly_temps$METRIC == 'medmintemp',] %>% lm(TEMP ~ WINTER, .)   ##### -208.0354 + 0.1157
long_monthly_temps[long_monthly_temps$MONTH == 1 & long_monthly_temps$METRIC == 'medmintemp',] %>% lm(TEMP ~ WINTER, .)   ##### -293.0626 + 0.1553
long_monthly_temps[long_monthly_temps$MONTH == 2 & long_monthly_temps$METRIC == 'medmintemp',] %>% lm(TEMP ~ WINTER, .)   ##### 70.70226 + -0.02528
long_monthly_temps[long_monthly_temps$MONTH == 3 & long_monthly_temps$METRIC == 'medmintemp',] %>% lm(TEMP ~ WINTER, .)   ##### -55.56912 + 0.04273




