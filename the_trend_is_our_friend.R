library(ggplot2)
library(scales)
library(RSQLite)
library(dplyr)
library(pracma)  ## Inclused a funcion to do various rolling means calc.
# theme_set(theme_linedraw() + theme(panel.grid = element_line(linetype = 'dashed')))
##
rm(list=ls())
db <- db <- dbConnect(RSQLite::SQLite(),dbname= "../COVID-19-DB/NYTimes.sqlite3")
df <- dbGetQuery(db,"select * from STATESDAILY")
df <- df[order(df$date),]
df$date <- as.Date(df$date)
df <- subset(df,state =="Ohio" & date >="2020-03-10" )
##
summary(df$new_cases,na.rm=TRUE)
summary(df$new_deaths,na.rm=TRUE)
# 
## Calculate Rolling Mean
#
df$mac7 <- movavg(df$new_cases,7, type="e")
df$mac14 <- movavg(df$new_cases,14, type="e")
##
ggplot(df) + geom_point(aes(x=date,y=new_cases))
ggplot(df) + geom_line(aes(x=date,y=mac7)) +
  labs(title="Ohio Cases: 7 Day Moving Average")
ggplot(df) + geom_line(aes(x=date,y=mac14)) +
  labs(title="Ohio Cases: 14 Day Moving Average")
##
df$mad7 <- movavg(df$new_deaths,7, type="e")
df$mad14 <- movavg(df$new_deaths,14, type="e")
df$mad28 <- movavg(df$new_deaths,28, type="e")
df$mad60 <- movavg(df$new_deaths,60, type="e")

ggplot(df) + geom_line(aes(x=date,y=mad7)) +
  labs(title="Ohio Deaths: 7 Day Moving Average") +
  ylim(0,90)
ggplot(df) + geom_line(aes(x=date,y=mad14)) +
  labs(title="Ohio Deaths: 14 Day Moving Average") +
  ylim(0,90)
## =
ggplot(df) + geom_line(aes(x=date,y=mad28)) +
  labs(title="Ohio Deaths: 28 Day Moving Average") +
  ylim(0,90)

ggplot(df) + geom_smooth(aes(x=date,y=new_deaths),span=0.25) +
  labs(title="Ohio Deaths: Loess: Span=0.25") 
  

### Benford's Analysis
library(benford.analysis)
cases <- df %>% dplyr::select(new_cases)
deaths <- df %>% dplyr::select(new_deaths)
##
bl_cases <- benford(cases$new_cases,1)
plot(bl_cases)
#
bl_cases2 <- benford(cases$new_cases,2)
plot(bl_cases2)
## Deaths
bl_deaths <- benford(deaths$new_deaths,1)
plot(bl_deaths)
#
bl_deaths2 <- benford(deaths$new_deaths,2)
plot(bl_deaths2)
