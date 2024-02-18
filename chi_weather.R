library(tidyverse)
library(lubridate)
library(magrittr)
library(ggplot2)
library(rvest)
library(weathermetrics)
library(stringr)


all_stations <- read_table('Data Science Practice/Weather/ghcnd-stations.txt', col_names = c('stationid', 'latitude', 'longitude', 'elevation', 'name', 'X6', 'X7', 'X8', 'X9'))
all_stations[all_stations %>% is.na] <- ''

all_stations$name <- paste(all_stations$name, all_stations$X6, all_stations$X7, all_stations$X8, all_stations$X9, sep = " ") #%>% length
all_stations <- all_stations[,c('stationid', 'latitude', 'longitude', 'elevation', 'name')]

all_stations[(all_stations$name %>% tolower) %>% grep('ohare', .),]$stationid
all_stations[all_stations$stationid %>% grep('USW', .),]




big_citiesdf <- (read_html('https://www.infoplease.com/us/cities/top-50-cities-us-population-and-rank') %>% html_nodes('table') %>% html_table(header = TRUE))[[1]]
big_citiesdf %<>% separate(., City, into = c('City', 'State'), sep = ', ')
big_citiesdf$State <- state.abb[match(big_citiesdf$State, state.name)]
big_citiesdf[big_citiesdf == 'New York City'] <- 'New York'
big_citiesdf$State[big_citiesdf$City == 'Washington'] <- 'DC'

big50_cities <- paste0(big_citiesdf$City, ', ', big_citiesdf$State)

wban_stations <- read.table('Data Science Practice/Weather/wbanmasterlist.psv', sep = '|', header = TRUE)
wban_stations <- wban_stations[, !names(wban_stations) %in% 'COMMENTS']
wban_stations$STATION_NAME[wban_stations$STATION_NAME == 'COLORADO SPINGS'] <- 'COLORADO SPRINGS'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'DFW'] <- 'DALLAS'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'ORF'] <- 'VIRGINIA BEACH'
wban_stations$STATION_NAME[wban_stations$CALL_SIGN == 'FTG'] <- 'AURORA'

wban_stations %<>% unite(STATION_NAME, STATION_NAME, STATE_PROVINCE, sep = ', ', remove = TRUE)

big50_stations <- wban_stations[wban_stations$STATION_NAME %in% (big50_cities %>% toupper()), ] #%>% group_by(STATION_NAME) %>% summarise(count = n()) %>% print(n = 50)


make_full_id <- function(id){
  id %<>% toString()
  len <- id %>% str_length()
  num_zeroes <- 8 - len
  
  zeroes <- replicate(num_zeroes, '0') %>% paste(collapse = '')
  
  full_id <- paste0('USW', zeroes, id)
  return(full_id)
}


big50_stations %<>% mutate(ghcn_id = sapply(big50_stations$WBAN_ID, make_full_id), .after = WBAN_ID)

big50_stations[big50_stations$ghcn_id %in% all_stations$stationid,] %>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n = 50)

final50_stations <- big50_stations[big50_stations$ghcn_id %in% all_stations$stationid & big50_stations$CALL_SIGN != '',] #%>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n = 50)

final50_stations %>% group_by(STATION_NAME) %>% summarise(n()) %>% print(n=50)
final50_stations[final50_stations$STATION_NAME == 'CHICAGO, IL',]

final50_stations#$ghcn_id[1:50]


stations <- final50_stations$ghcn_id[1:50]
stations <- stations %>% paste0(collapse = ',')
chicago_station <- 'USW00094846'
startdate <- '1970-01-01'
enddate <- today()

paste0('https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&stations=', chicago_station,
       '&startDate=', startdate, '&endDate=', enddate, '&boundingBox=90,-180,-90,180')

read_csv("https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries&stations=USW00094846&startDate=1970-01-01&endDate=2024-02-13&boundingBox=90,-180,-90,180") %>% tail()

weather_data <- read_csv('Data Science Practice/Weather/daily-summaries-2024-01-28T14-30-46.csv')
weather_data %<>% select(where(function(x) any(!is.na(x))))
weather_data %<>% select(-PGTM)
weather_data[,c('TAVG', 'TMAX', 'TMIN', 'ADPT', 'AWBT')] <- (weather_data[,c('TAVG', 'TMAX', 'TMIN', 'ADPT', 'AWBT')] / 10) %>% celsius.to.fahrenheit()
weather_data[,c('AWND', 'WSF2', 'WSF5')] <- weather_data[,c('AWND', 'WSF2', 'WSF5')]*.2237

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

###Median temp across years for each month
med_avg_df <- winter_data %>% group_by(WINTER, MONTH = month(DATE)) %>% summarize(medmaxtemp = median(TMAX), medmintemp = median(TMIN), avgmaxtemp = mean(TMAX), avgmintemp = mean(TMIN)) #grouped df with median maxtemp for each month & year
long_monthly_temps <- med_avg_df %>% pivot_longer(-c(WINTER, MONTH), names_to = 'METRIC', values_to = 'TEMP')


ggplot(data = transform(long_monthly_temps[long_monthly_temps$MONTH %in% c(12, 1, 2, 3),], MONTH = factor(MONTH, levels = c(12, 1, 2, 3))), mapping = aes(TEMP, x = WINTER, color = METRIC)) + geom_line() + facet_wrap(vars(MONTH), labeller = as_labeller(c('12' = 'December', '1' = 'January', '2' = 'February', '3' = 'March')), scales = "free") + 
  geom_smooth(method = "lm", se = FALSE) + scale_y_continuous(limits = c(15, 70))

ggplot(data = transform(med_max_df[med_min_df$MONTH %in% c(12, 1, 2, 3),], MONTH = factor(MONTH, levels = c(12, 1, 2, 3))), mapping = aes(medtemp, x = WINTER)) + geom_line() + facet_wrap(vars(MONTH)) + geom_smooth(method = "lm", se = FALSE)


med_max_df[med_max_df$MONTH == 12,] %>% lm(medtemp ~ WINTER, .)
med_max_df[med_max_df$MONTH == 1,] %>% lm(medtemp ~ WINTER, .)
med_max_df[med_max_df$MONTH == 2,] %>% lm(medtemp ~ WINTER, .)
med_max_df[med_max_df$MONTH == 3,] %>% lm(medtemp ~ WINTER, .)

med_avg_df %>% pivot_longer(-c(WINTER, MONTH), names_to = 'METRIC', values_to = 'TEMP')



