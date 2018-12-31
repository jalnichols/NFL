
library(tidyverse)

#

games_to_download <- readr::read_csv("C:/Users/Jake/Downloads/big-data-bowl/games-ids.csv")

tracking_data_list <- vector("list", length(games_to_download$game_id))

#

for(g in 1:length(games_to_download$game_id)) {
  
  path <- paste0("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/", games_to_download$game_id[[g]])
  
  d <- readr::read_csv(path)
  
  tracking_data_list[[g]] <- d
  
}

#

play_data <- readr::read_csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv")

#

tracking_data <- bind_rows(tracking_data_list) %>%
  
  left_join(
    
    play_data, by = c("playId","gameId")
    
  )

#