
library(tidyverse)
library(rvest)
library(shiny)

#

list_years <- vector("list", 26)

for(y in 1:26) {
  
  df <- paste0("https://www.pro-football-reference.com/years/", 1993 + y, "/draft.htm") %>%
    
    read_html() %>%
    html_nodes('table') %>%
    html_table() %>%
    .[[1]] %>%
    janitor::clean_names() %>%
    mutate(draft_year = y + 1993)
  
  list_years[[y]] <- df

}

#

drafted_players <- bind_rows(list_years) %>%
  select(draft_year,
         round = x,
         pick = x_2,
         team = x_3,
         player = x_4,
         position = x_5,
         age = x_6,
         retired = x_7,
         AV = approx_val,
         games = x_9,
         college = x_13) %>%
  
  filter(!age == "Age") %>%
  
  mutate(retired = as.numeric(retired),
         age = as.numeric(age),
         games = as.numeric(games),
         AV = as.numeric(AV),
         round = as.numeric(round),
         pick = as.numeric(pick)) %>%
  
  mutate(AV = ifelse(is.na(AV), 0, AV),
         games = ifelse(is.na(games), 0, games),
         career = (retired - draft_year) + 1,
         career = ifelse(is.na(career), 0, career),
         AV_season = AV / games * 16,
         player = str_replace(player, "HOF", ""),
         AV_regressed = (AV_season * games) / (games + 32)) %>%
  
  mutate(AV_season = ifelse(AV_season == "NaN", 0, AV_season),
         AV_regressed = ifelse(AV_regressed == "NaN", 0, AV_regressed)) %>%

  group_by(round) %>%
  mutate(round_baseline = sum(AV, na.rm = T) / sum(career, na.rm = T)) %>%
  ungroup() %>%
  
  mutate(relative_round = AV_regressed - round_baseline) %>%
  
  mutate(position2 = ifelse(position %in% c("OLB", "ILB"), "LB",
                            ifelse(position %in% c("NT", "DT"), "DT",
                                   ifelse(position %in% c("FB", "RB"), "RB",
                                          ifelse(position %in% c("DB", "S", "CB"), "DB", position)))))

#

drafted_players$position2 <- factor(drafted_players$position2, levels = c("QB", "RB", "WR", "TE", "C", "G", "T",
                                                                         "DE", "DT", "LB", "DB", "K", "P", "LS"))

#

write_csv(drafted_players, "Shiny_Draft/data/drafted_players.csv")