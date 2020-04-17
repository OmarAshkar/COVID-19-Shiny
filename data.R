library(tidyverse)
# Download data 
## Get zip file
#download.file(url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip" 
#, destfile = "data/covid_repo.zip") 

## Unzip file 
#unzip("data/covid_repo.zip", exdir = "data")


datanames <- list.files("data/COVID-19-master/csse_covid_19_data/csse_covid_19_daily_reports", 
                        pattern = "\\.csv$",
                        full.names = T)


for(i in 1:60){
    # fname <-  str_extract(string = datanames, 
    #                       pattern = pat)[i]
    assign("fname",
           read.csv(datanames[i])
    )
    if(i == 1){
        x = fname
    }
    else {
        x <- bind_rows(x, fname)
    }
}

for(i in 61:83){
    # fnamey <-  str_extract(string = datanames, 
    #                       pattern = pat)[i]
    assign("fname",
           read.csv(datanames[i])
    )
    if(i == 61){
        y = fname
    }
    else {
        y <- bind_rows(y, fname)
    }
}
y <- y %>% rename(Last.Update = Last_Update, Province.State = Province_State,
                  Latitude = Lat, Longitude = Long_, Country.Region = Country_Region)

dat <- bind_rows(x, y) %>% select(2:6, 11)

dat$Last.Update <- as.Date(lubridate::parse_date_time(dat$Last.Update, c("%m/%d/%Y %H:%M", "%m/%d/%y %H:%M", "%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M","%Y-%m-%d %H:%M:%S")))
    