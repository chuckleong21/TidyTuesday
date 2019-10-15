# get a local copy of the data for this week to save time on getCurl
# pkgs & data ------
library(tidyverse)

# fast read
data.table::fread(here::here("Week 42", "epacars.csv"))