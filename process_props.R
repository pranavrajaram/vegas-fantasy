library(tidyverse)
library(ffscrapr)
library(nflreadr)
library(DBI)

# Read CSV
df <- read_csv("nfl_season_props_master.csv") %>%
  mutate(player = dp_cleannames(player))


# ---- Convert American odds to probability ----
american_to_prob <- function(odds) {
  if (is.na(odds)) return(NA_real_)
  odds <- suppressWarnings(as.numeric(str_trim(as.character(odds))))
  if (is.na(odds)) return(NA_real_)
  
  if (odds < 0) {
    return(-odds / (-odds + 100))
  } else {
    return(100 / (odds + 100))
  }
}

# ---- Expected value from line and odds ----
expected_from_line <- function(line, over_odds, under_odds) {
  if (is.na(line)) return(NA_real_)
  line <- suppressWarnings(as.numeric(str_trim(as.character(line))))
  if (is.na(line)) return(NA_real_)
  
  p_over <- american_to_prob(over_odds)
  p_under <- american_to_prob(under_odds)
  
  if (is.na(p_over) & is.na(p_under)) return(NA_real_)
  if (is.na(p_over)) p_over <- 1 - p_under
  if (is.na(p_under)) p_under <- 1 - p_over
  
  return(p_over * ceiling(line) + p_under * floor(line))
}


df_clean <- df %>%
  group_by(player) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "{.col}"), .groups = "drop") %>%
  ungroup()

# Calculate expected values
prefixes <- c("rush_yds", "rush_tds", "rec_yds", "rec_tds", "pass_yds", "pass_tds")

for (prefix in prefixes) {
  line_col <- paste0(prefix, "_line")
  over_col <- paste0(prefix, "_over_price")
  under_col <- paste0(prefix, "_under_price")
  exp_col <- paste0(prefix, "_expected")
  
  df_clean[[exp_col]] <- mapply(
    expected_from_line,
    df_clean[[line_col]],
    df_clean[[over_col]],
    df_clean[[under_col]]
  )
}

# ---- Calculate implied fantasy points
df_clean <- df_clean %>%
  mutate(implied_fantasy_points =
           coalesce(rush_yds_expected, 0) * 0.1 +
           coalesce(rec_yds_expected, 0) * 0.1 +
           (coalesce(rush_tds_expected, 0) + coalesce(rec_tds_expected, 0)) * 6 +
           coalesce(pass_yds_expected, 0) * 0.04 +
           coalesce(pass_tds_expected, 0) * 4
  ) %>%
  arrange(desc(implied_fantasy_points))


rosters <- nflreadr::load_rosters() %>%
  select(full_name, team, position, headshot_url) %>%
  mutate(player = dp_cleannames(full_name)) %>%
  select(-full_name)

df_merged <- df_clean %>%
  left_join(rosters, by = "player") %>%
  select(player, team, position, headshot_url, everything()) %>%
  mutate(date = Sys.Date())

today <- Sys.Date() 

write.csv(df_merged, paste0("data/implied_fp_", gsub("-", "_", today), ".csv"))

write.csv(df_merged, "data/implied_fp_latest.csv")
