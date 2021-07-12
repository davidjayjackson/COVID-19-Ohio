library(ggplot2)
library(scales)
library(dplyr)
library(dlookr)
library(forecast)
library(flextable)
library(RSQLite)
theme_set(theme_linedraw())
rm(list=ls())
db <- db <- dbConnect(RSQLite::SQLite(),dbname= "../COVID-19-DB/NYTimes.sqlite3")
df <- dbGetQuery(db,"select date,new_cases,new_deaths from COUNTYDAILY ")
df$date <- as.Date(df$date)

median_counts <- df %>% group_by(date) %>%
  summarise(cases_median = median(new_cases),
            cases_mean = mean(new_cases),
            deaths_median = median(new_deaths),
            deaths_mean = mean(new_deaths))

ggplot(median_counts) + geom_line(aes(x=date,y=cases_median,col="Median")) +
  scale_y_continuous(labels = comma) +
  labs(title="US Daily Median Cases")

ggplot(median_counts) + geom_line(aes(x=date,y=cases_mean,col="Mean")) +
  scale_y_continuous(labels = comma)
  labs(title="US Daily Mean Cases")
  
  ggplot(median_counts) + geom_line(aes(x=date,y=deaths_median,col="Median")) +
    scale_y_continuous(labels = comma) +
    labs(title="US Daily Median Deaths")
  
  ggplot(median_counts) + geom_line(aes(x=date,y=deaths_mean,col="Mean")) +
    scale_y_continuous(labels = comma)
  labs(title="US Daily Mean Deaths")