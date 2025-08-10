library(dplyr) 
library(jsonlite) 

scrape_rotowire <- function(url) {
  today <- Sys.Date()
  
  result <- fromJSON(txt = url)
  
  df <- result %>% 
    mutate(date = today) %>%
    select(name, team, date, fanduel_line, fanduel_odds, fanduel_odds_under)
  
  return(df)
}

pass_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Pass%20Yards"
rush_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rush%20Yards"
rec_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rec%20Yards"

pass_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Pass%20TD"
rush_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rush%20TD"
rec_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rec%20TD"

pass_yards_df <- scrape_rotowire(pass_yards_url) %>%
  rename(player = name, pass_yds_line = fanduel_line, pass_yds_over_price = fanduel_odds, pass_yds_under_price = fanduel_odds_under)

rush_yards_df <- scrape_rotowire(rush_yards_url) %>%
  rename(player = name, rush_yds_line = fanduel_line, rush_yds_over_price = fanduel_odds, rush_yds_under_price = fanduel_odds_under)

rec_yards_df <- scrape_rotowire(rec_yards_url) %>%
  rename(player = name, rec_yds_line = fanduel_line, rec_yds_over_price = fanduel_odds, rec_yds_under_price = fanduel_odds_under)

pass_tds_df <- scrape_rotowire(pass_tds_url) %>%
  rename(player = name, pass_tds_line = fanduel_line, pass_tds_over_price = fanduel_odds, pass_tds_under_price = fanduel_odds_under)

rush_tds_df <- scrape_rotowire(rush_tds_url) %>%
  rename(player = name, rush_tds_line = fanduel_line, rush_tds_over_price = fanduel_odds, rush_tds_under_price = fanduel_odds_under)

rec_tds_df <- scrape_rotowire(rec_tds_url) %>%
  rename(player = name, rec_tds_line = fanduel_line, rec_tds_over_price = fanduel_odds, rec_tds_under_price = fanduel_odds_under)

df_full <- pass_yards_df %>%
  full_join(rush_yards_df, by = c("player", "team", "date")) %>%
  full_join(rec_yards_df, by = c("player", "team", "date")) %>%
  full_join(pass_tds_df, by = c("player", "team", "date")) %>%
  full_join(rush_tds_df, by = c("player", "team", "date")) %>%
  full_join(rec_tds_df, by = c("player", "team", "date")) %>%
  # convert character columns to numeric except for player date and team
  mutate(across(where(is.character) & !c(player, team, date), as.numeric))


# write dataframe to .csv in a folder called "data/"
write.csv(df_full, 'data/roto_props_master.csv', row.names = F)
