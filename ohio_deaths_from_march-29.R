library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(RSQLite)
## Read Data from NYTimes DB

rm(list=ls())
db <- db <- dbConnect(RSQLite::SQLite(),dbname= "../COVID-19-DB/NYTimes.sqlite3")
df <- dbGetQuery(db,"select * from STATESDAILY where state ='Ohio'")
df$date <- as.Date(df$date)
df$new_deaths <- as.double(df$new_deaths)
df <- df %>% filter(date >="2020-10-01") %>% select(date,new_cases,new_deaths)
## Plot Raw Daily Deaths

df <- dplyr::mutate(
  df,new_deaths1 = dplyr::if_else(date >= "2020-10-01" & date <="2020-12-31",new_deaths + 30, new_deaths))

df %>% filter(date >="2020-10-01" & date <="2020-12-31" ) %>%
ggplot() + geom_line(aes(x=date,y=new_deaths1,col="Deaths1")) + 
  geom_line(aes(x=date,y=new_deaths,col="Deaths")) +
    labs(title = "Fixed Two Dates in Feb. 2021: Daily Deaths")
  
## Month of March
march <- df %>% filter(date >="2021-01-01")
ggplot(march) + geom_line(aes(x=date,y=new_deaths)) + ylim(0,150)
## Seven Day Moving Average
df$MAD <- forecast::ma(df$new_deaths1,14,centre = TRUE)
df %>%filter(date >="2021-02-14") %>%
ggplot() + geom_line(aes(x=date,y=MAD)) +
  labs(title = "Seven  Day Moivng Average Deaths") + ylim(0,125)
