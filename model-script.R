## loading packages, setting global presets ----

library(tidyverse)
library(nbastatR)
library(rvest)
library(gt)
library(espnscrapeR)
library(RColorBrewer)
library(reactable)

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

ncaa_2020 <- read_csv("ncaa-2020.csv") %>% mutate(Year = 2020)
ncaa_2021 <- read_csv("ncaa-2021.csv") %>% mutate(Year = 2021)

ncaa_data <- rbind(ncaa_2021,ncaa_2020)
ncaa_data <- ncaa_data[!duplicated(ncaa_data$NAME),]

rm(ncaa_2020)
rm(ncaa_2021)

## merging NBA and NCAA data ---

total_data <- left_join(ncaa_data,nba_data, by = c("NAME" = "Player")) %>%
  filter(is.na(fg3m) == F) %>%
  mutate(sq_name = gsub(" ","%20",NAME),
         sq_school = gsub(" ","%20",SCHOOL)) %>%
  mutate(sq_url = paste0("https://shotquality.com/individual-player/college_mens/",Year,
                         "/",sq_school,
                         "/",sq_name)) %>%
  select(-sq_name,-sq_school)

## importing ShotQuality data ----

sq_data <- read_csv("sq-data.csv") %>% select(NAME,`sq-3pt-catch`,`sq-3pt-dribble`) %>%
  rename(sq_3pt_catch = `sq-3pt-catch`,sq_3pt_dribble = `sq-3pt-dribble`)

## merging ShotQuality data to NBA and NCAA data ----

total_data <- total_data %>% inner_join(sq_data)

## building regression models ----

regression_data <- total_data %>%
  select(fg3pct,pctFT,pct3P,sq_3pt_catch,sq_3pt_dribble)  %>% filter(is.na(sq_3pt_dribble) == F)

regression_data_dribble <- regression_data %>% filter(is.na(sq_3pt_dribble) == F)

model <- lm(fg3pct~.,regression_data_dribble)

model.sq.catch <- lm(fg3pct~sq_3pt_catch,regression_data)

model.sq.dribble <- lm(fg3pct~sq_3pt_dribble,regression_data_dribble)
  
model.ncaa.ft <- lm(fg3pct~pctFT,regression_data)

model.ncaa.3p <- lm(fg3pct~pct3P,regression_data)

model.sqdata <- lm(fg3pct~sq_3pt_catch+sq_3pt_dribble,regression_data_dribble)

model.ncaadata <- lm(fg3pct~pct3P+pctFT,regression_data)

rsquared <- function(model) {
  r <- round(summary(model)$r.squared,3)
  r
}

adjrsquared <- function(model) {
  adjr <- round(summary(model)$adj.r.squared,3)
  adjr
}

mse2 <- function(model){
  mse2 = round(sqrt(mean(model$residuals^2)),4)
  mse2
}
model.summariser <- function(name,model){
  name = name
  r = rsquared(model)
  adjr = adjrsquared(model)
  rmse = mse2(model)
  output <- data.frame(Model = name,RSquared = r, AdjRSquared = adjr,RootMeanSqError = rmse)
  output
}

ncaaf.r <- model.summariser("NCAA FT%",model.ncaa.ft)
ncaat.r <- model.summariser("NCAA 3P%",model.ncaa.3p)
ncaa.r <- model.summariser("NCAA FT% & 3P%",model.ncaadata)

full.r <- model.summariser("All Variables",model)

sq.r <- model.summariser("SQ Catch & Dribble",model.sqdata)
sqc.r <- model.summariser("SQ Catch",model.sq.catch)
sqd.r <- model.summariser("SQ Dribble",model.sq.dribble)

r.table <- rbind(ncaaf.r,ncaat.r,ncaa.r,sq.r,sqc.r,sqd.r,full.r) %>%
  arrange(-AdjRSquared) %>% 
  select(-RSquared)

r.export.table <- gt(r.table) %>%
  cols_label(AdjRSquared = "Adj. R-Squared",RootMeanSqError = "Root MSE") %>%
  espnscrapeR::gt_theme_538() %>%
  data_color(columns = c(AdjRSquared),
             colors = brewer.pal(9,"RdYlGn")) %>%
  data_color(columns = c(RootMeanSqError),
             colors = rev(brewer.pal(9,"RdYlGn")))

## using regression models for prediction
## creating a final dataset

prediction_data <- total_data %>% 
  select(-RK,-PICK,-YR,-sq_url,-fg3per36,-Year) %>% 
  filter(is.na(sq_3pt_dribble) == F)

prediction_data <- prediction_data %>%
  mutate(all_predictors = round(predict(model,prediction_data),3),
         sq_composite = round(predict(model.sqdata,prediction_data),3),
         ncaa_3p_only = round(predict(model.ncaa.3p,prediction_data),3),
         fg3pct = round(fg3pct,3)) %>%
  rename(Name = NAME, School = SCHOOL, Conference = CONF,
         NCAA_FT_pct = pctFT,NCAA_3P_per_100 = per1003p,
         NCAA_3P_pct = pct3P, NBA_3PM = fg3m, NBA_3PA = fg3a,
         NBA_3P_pct = fg3pct,SQ_3P_catch_PPP = sq_3pt_catch,
         SQ_3P_dribble_PPP = sq_3pt_dribble) %>%
  mutate(SQ_3P_catch_pct = round(SQ_3P_catch_PPP/3,3),SQ_3P_dribble_pct = round(SQ_3P_dribble_PPP/3,3)) %>%
  select(Name,School,Conference,NCAA_FT_pct,NCAA_3P_pct,
         SQ_3P_catch_pct,SQ_3P_dribble_pct,all_predictors,sq_composite,
         ncaa_3p_only,NBA_3P_pct)


table <- prediction_data %>%
  arrange(-NBA_3P_pct) %>%
  gt() %>% espnscrapeR::gt_theme_538() %>%
  tab_header(title = md("**Testing NBA Three-Point Percentage Prediction Models**")) %>%
  data_color(columns = c(all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
             colors = brewer.pal(9,"RdYlGn")) %>%
  cols_width(Name ~ px(250),
             School ~ px(200)) %>%
  cols_label(NCAA_FT_pct = "NCAA FT%",
             NCAA_3P_pct = "NCAA 3P%",
             SQ_3P_catch_pct = "SQ Catch 3P%",
             SQ_3P_dribble_pct = "SQ Dribble 3P%",
             all_predictors = "Prediction using all variables",
             sq_composite = "Prediction using SQ Data Only",
             ncaa_3p_only = "Prediction using NCAA 3P%",
             NBA_3P_pct = "NBA 3P%",
             Conference = "Conf.") %>%
  fmt_percent(columns = c(NCAA_FT_pct,NCAA_3P_pct,SQ_3P_catch_pct,SQ_3P_dribble_pct,
                          all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
              decimals = 1) %>%
  tab_source_note("NBA data source: Basketball-Reference") %>%
  tab_source_note("NCAA data source: BartTorvik") %>%
  tab_source_note("ShotQuality data source: ShotQuality.com")


table

gtsave(table,"export-table.png")
gtsave(table,"export-table.html")
gtsave(r.export.table,"model-comparison.html")
