library(ggplot2)
library(scales)
library(RSQLite)
library(dplyr)
library(pracma)

### State Data
rm(list=ls())
db <- db <- dbConnect(RSQLite::SQLite(),dbname= "../COVID-19-DB/NYTimes.sqlite3")
state <- dbGetQuery(db,"select date,new_cases,new_deaths from STATESDAILY where state='Ohio'")
state <- state %>% filter(date >="202003-10")
state <- state[order(state$date),]
state$date <- as.Date(state$date)
summary(state)

## County Data
county <- dbGetQuery(db,"select date,new_cases,new_deaths from  COUNTYDAILY where state='Ohio'")
county$date <- as.Date(county$date)
county <- state[order(county$date),]
summary(county)

county_sum <- county %>% group_by(date) %>% 
  summarise(Cases = sum(new_cases),
            Deaths = sum(new_deaths))

# Plot Cases by Day using both State and county daily totals

ggplot() + geom_line(data=state,aes(x=date,y=new_cases,col="State Totals")) +
  geom_line(data=county_sum,aes(x=date,y=Cases,col="Country Totals")) + 
  scale_y_continuous(labels = comma)

ggplot() + geom_line(data=state,aes(x=date,y=new_deaths,col="State Totals")) +
  ylim(0,500)
  
ggplot() + geom_line(data=county_sum,aes(x=date,y=Deaths,col="County Totals"))
