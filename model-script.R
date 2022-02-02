## loading packages, setting global presets ----

library(tidyverse)
library(nbastatR)
library(rvest)

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

bbref.draft.scraper <- function(pageurl,yr){
  site <- read_html(pageurl)
  table <- site %>% html_elements(".sortable") %>% html_table()
  table <- data.frame(table)
  colnames(table) = unlist(table[1,]) 
  names(table)[8:11] = c("TOTMP","TOTPTS","TOTTRB","TOTAST")
  table <- table %>% mutate(DraftYr = yr) %>%
    mutate(Rk = as.numeric(Rk),Pk = as.numeric(Pk), Yrs = as.numeric(Yrs),
           G = as.numeric(G),TOTMP = as.numeric(TOTMP), TOTPTS = as.numeric(TOTPTS),
           TOTTRB = as.numeric(TOTTRB), TOTAST = as.numeric(TOTAST), `FG%` = as.numeric(`FG%`),
           `3P%` = as.numeric(`3P%`),`FT%` = as.numeric(`FT%`),
           MPG = as.numeric(MP), PPG = as.numeric(PTS), RPG = as.numeric(TRB),
           APG = as.numeric(AST), WS = as.numeric(WS), `WS/48` = as.numeric(`WS/48`),
           BPM = as.numeric(BPM), VORP = as.numeric(VORP)) %>%
    mutate(Tm = gsub("CHH","CHA",Tm)) %>%
    mutate(Tm = gsub("CHO","CHA",Tm)) %>%
    mutate(Tm = gsub("NOH","NOP",Tm)) %>%
    mutate(Tm = gsub("NJN","BRK",Tm)) %>%
    mutate(Tm = gsub("SEA","OKC",Tm)) %>%
    mutate(Tm = gsub("VAN","MEM",Tm)) %>%
    mutate(Tm = gsub("NOK","CHA",Tm)) %>%
    mutate(Tm = gsub("WSB","WAS",Tm)) %>%
    mutate(NAtest = is.na(Pk))
  
  table <- table %>% filter(NAtest == FALSE) %>% select(-PTS,-TRB,-MP,-AST,-NAtest)
  table
  }

draft.2021 <- bbref.draft.scraper("https://www.basketball-reference.com/draft/NBA_2021.html",2021)
draft.2020 <- bbref.draft.scraper("https://www.basketball-reference.com/draft/NBA_2020.html",2020)
draft_data <- rbind(draft.2021,draft.2020)

rm(draft.2021)
rm(draft.2020)

## combining data ----

full_data <- left_join(draft_data,nba_data) %>% filter(is.na(fg3m) == F)


