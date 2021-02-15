library(ggplot2)
library(scales)
library(plotly)
library(pracma)
rm(list=ls())
df <- read.csv("./adjusted-deaths.csv")
df$date <- as.Date(df$date,format="%Y-%m-%d")
ggplot(df) + geom_line(aes(x=date,y=new_deaths,col="Deaths")) +
  geom_line(aes(x=date,y=adjusted,col="Adjusted Deaths"))
##
df$MDDeaths <- movavg(df$new_deaths,30,type="w")
df$MDAdjusted <- movavg(df$adjusted,30,type="w")
##
ggplot(df) + geom_line(aes(x=date,y=MDDeaths,col="Deaths")) +
  geom_line(aes(x=date,y=MDAdjusted,col="Ajusted"))
