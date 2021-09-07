# COVID and start of colleges - Fall 2021
library(tidyverse)
library(readxl)
 
x <- read_xlsx("data/colleges.xlsx")

cases <- read_csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")
