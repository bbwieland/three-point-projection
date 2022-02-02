## loading packages, setting global presets ----

library(tidyverse)
library(nbastatR)
library(rvest)
library(bigballR)
library(hoopR)

setwd("~/Desktop/SQ Projects/three-point-projection")

## scraping and calculating the NBA data ----

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
nbastatR::assign_nba_players()
nba_data <- nbastatR::bref_players_stats(seasons = 2019:2022) %>%
  group_by(namePlayer) %>%
  summarise(fg3m = sum(fg3mTotals),fg3a = sum(fg3aTotals),
            fg3per36 = (sum(fg3a)/sum(minutesTotals))*36,
            fg3pct = fg3m/fg3a) %>%
  filter(fg3a >= 50) %>%
  rename(Player = namePlayer)

rm(dataBREFPlayerAdvanced)
rm(dataBREFPlayerTotals)
rm(df_dict_nba_players)

## scraping and calculating college data ----

ncaa_2020 <- read_csv("ncaa-2020.csv")
ncaa_2021 <- read_csv("ncaa-2021.csv")

ncaa_data <- rbind(ncaa_2021,ncaa_2020)
ncaa_data <- ncaa_data[!duplicated(ncaa_data$NAME),]

rm(ncaa_2020)
rm(ncaa_2021)

## merging NBA and NCAA data ---

total_data <- left_join(ncaa_data,nba_data, by = c("NAME" = "Player")) %>%
  filter(is.na(fg3m) == F)

