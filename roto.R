library(dplyr)
library(jsonlite)

year_dir <- file.path("data", format(Sys.Date(), "%Y"))
dir.create(year_dir, showWarnings = FALSE, recursive = TRUE)

scrape_rotowire <- function(url) {
  today <- Sys.Date()
  
  result <- fromJSON(txt = url)
  
  df <- result %>%
    mutate(
      date = today,
      line       = dplyr::coalesce(fanduel_line,       caesars_line),
      odds_over  = dplyr::coalesce(fanduel_odds,       caesars_odds),
      odds_under = dplyr::coalesce(fanduel_odds_under, caesars_odds_under)
    ) %>%
    select(name, team, date, line, odds_over, odds_under)
  
  return(df)
}

pass_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Pass%20Yards"
rush_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rush%20Yards"
rec_yards_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rec%20Yards"

pass_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Pass%20TD"
rush_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rush%20TD"
rec_tds_url <- "https://www.rotowire.com/betting/nfl/tables/player-futures.php?future=Rec%20TD"

pass_yards_df <- scrape_rotowire(pass_yards_url) %>%
  rename(player = name, pass_yds_line = line, pass_yds_over_price = odds_over, pass_yds_under_price = odds_under)

rush_yards_df <- scrape_rotowire(rush_yards_url) %>%
  rename(player = name, rush_yds_line = line, rush_yds_over_price = odds_over, rush_yds_under_price = odds_under)

rec_yards_df <- scrape_rotowire(rec_yards_url) %>%
  rename(player = name, rec_yds_line = line, rec_yds_over_price = odds_over, rec_yds_under_price = odds_under)

pass_tds_df <- scrape_rotowire(pass_tds_url) %>%
  rename(player = name, pass_tds_line = line, pass_tds_over_price = odds_over, pass_tds_under_price = odds_under)

rush_tds_df <- scrape_rotowire(rush_tds_url) %>%
  rename(player = name, rush_tds_line = line, rush_tds_over_price = odds_over, rush_tds_under_price = odds_under)

rec_tds_df <- scrape_rotowire(rec_tds_url) %>%
  rename(player = name, rec_tds_line = line, rec_tds_over_price = odds_over, rec_tds_under_price = odds_under)

df_full <- pass_yards_df %>%
  full_join(rush_yards_df, by = c("player", "team", "date")) %>%
  full_join(rec_yards_df, by = c("player", "team", "date")) %>%
  full_join(pass_tds_df, by = c("player", "team", "date")) %>%
  full_join(rush_tds_df, by = c("player", "team", "date")) %>%
  full_join(rec_tds_df, by = c("player", "team", "date")) %>%
  # convert character columns to numeric except for player date and team
  mutate(across(where(is.character) & !c(player, team, date), as.numeric))


# write dataframe to .csv in the current year's data folder
write.csv(df_full, file.path(year_dir, "roto_props_master.csv"), row.names = F)
