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

regression_data <- regression_data %>% filter(is.na(sq_3pt_dribble) == F)

model <- lm(fg3pct~.,regression_data)

model.sq.catch <- lm(fg3pct~sq_3pt_catch,regression_data)

model.sq.dribble <- lm(fg3pct~sq_3pt_dribble,regression_data)
  
model.ncaa.ft <- lm(fg3pct~pctFT,regression_data)

model.ncaa.3p <- lm(fg3pct~pct3P,regression_data)

model.sqdata <- lm(fg3pct~sq_3pt_catch+sq_3pt_dribble,regression_data)

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
             colors = rev(brewer.pal(9,"RdYlGn"))) %>%
  tab_source_note("NBA data source: Basketball-Reference") %>%
  tab_source_note("NCAA data source: BartTorvik") %>%
  tab_source_note("ShotQuality data source: ShotQuality.com")%>%
  tab_header(title = md("**Comparing NBA Three-Point Prediction Models**"))

rm(ncaa.r)
rm(ncaaf.r)
rm(ncaat.r)
rm(sq.r)
rm(sqc.r)
rm(sqd.r)
rm(full.r)

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
         ncaa_3p_only,NBA_3P_pct) %>%
  select(-School,-Conference)


player.table <- prediction_data %>%
  arrange(-NBA_3P_pct) %>%
  gt() %>% espnscrapeR::gt_theme_538() %>%
  tab_header(title = md("**Comparing Predictions to NBA Performance**")) %>%
  data_color(columns = c(all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
             colors = brewer.pal(9,"RdYlGn")) %>%
  cols_width(Name ~ px(250))%>%
  cols_label(NCAA_FT_pct = "NCAA FT%",
             NCAA_3P_pct = "NCAA 3P%",
             SQ_3P_catch_pct = "SQ Catch 3P%",
             SQ_3P_dribble_pct = "SQ Dribble 3P%",
             all_predictors = "Prediction using all variables",
             sq_composite = "Prediction using SQ Data Only",
             ncaa_3p_only = "Prediction using NCAA 3P%",
             NBA_3P_pct = "NBA 3P%") %>%
  fmt_percent(columns = c(NCAA_FT_pct,NCAA_3P_pct,SQ_3P_catch_pct,SQ_3P_dribble_pct,
                          all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
              decimals = 1) %>%
  tab_source_note("NBA data source: Basketball-Reference") %>%
  tab_source_note("NCAA data source: BartTorvik") %>%
  tab_source_note("ShotQuality data source: ShotQuality.com")

## outlier table ----

outlier.table <- prediction_data %>%
  mutate(sq_miss = NBA_3P_pct - sq_composite,
         ncaa_miss = NBA_3P_pct - ncaa_3p_only,
         all_miss = NBA_3P_pct - all_predictors) %>%
  mutate(total_miss = sq_miss + ncaa_miss + all_miss) %>%
  mutate(avg_miss = round(total_miss/3,3)) %>%
  arrange(-avg_miss) %>%
  select(-NCAA_FT_pct,-NCAA_3P_pct,-SQ_3P_catch_pct,-SQ_3P_dribble_pct,
         -sq_miss,-ncaa_miss,-all_miss,-total_miss)

outlier.top.gt <- head(outlier.table,5) %>% gt() %>%
  cols_label(all_predictors = "Prediction using all variables",
             sq_composite = "Prediction using SQ Data Only",
             ncaa_3p_only = "Prediction using NCAA 3P%",
             NBA_3P_pct = "NBA 3P%",
             avg_miss = "Average Model Error")  %>% 
  espnscrapeR::gt_theme_538() %>%
  tab_header(title = md("**Who Overperformed the Models?**")) %>%
  fmt_percent(columns = c(all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
              decimals = 1) %>%
  data_color(columns = avg_miss,
             colors = brewer.pal(5,"Greens")) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = avg_miss))

outlier.bottom.gt <- tail(outlier.table,5) %>% arrange(avg_miss) %>% gt() %>%
  cols_label(all_predictors = "Prediction using all variables",
             sq_composite = "Prediction using SQ Data Only",
             ncaa_3p_only = "Prediction using NCAA 3P%",
             NBA_3P_pct = "NBA 3P%",
             avg_miss = "Average Model Error")  %>% 
  espnscrapeR::gt_theme_538() %>%
  tab_header(title = md("**Who Underperformed the Models?**")) %>%
  fmt_percent(columns = c(all_predictors,sq_composite,ncaa_3p_only,NBA_3P_pct),
              decimals = 1) %>%
  data_color(columns = avg_miss,
             colors = c("#9B111E","#D10000","#FF2400","#FF5C5C","#FF8A8A")) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = avg_miss))

## saving tables ----

gtsave(player.table,"model-predictions.png")
gtsave(player.table,"model-predictions.html")

gtsave(r.export.table,"model-comparison.html")
gtsave(r.export.table,"model-comparison.png")

gtsave(outlier.top.gt,"outliers-best.png")
gtsave(outlier.top.gt,"outliers-best.html")

gtsave(outlier.bottom.gt,"outliers-worst.png")
gtsave(outlier.bottom.gt,"outliers-worst.html")
