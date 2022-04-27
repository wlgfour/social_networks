#### Preamble ####
# Purpose: 
# Author: William Gerecke
# Email: wlgfour@gmail.com
# Date: 4/26/2022
# Prerequisites: R software
# Notes:


dfs <- map(list.files('outputs/raw', full.names=T), read.csv)
df <- bind_rows(dfs) |>
  select(!X)










