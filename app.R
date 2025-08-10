library(shiny)
library(dplyr)
library(readr)
library(gt)
library(gtExtras)
library(tidyr)
library(lubridate)
library(stringr)

# --- Data ---
owner <- "pranavrajaram"
repo  <- "vegas-fantasy"
branch <- "main"
base_raw <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/", branch, "/data/")

raw_latest <- paste0(base_raw, "implied_fp_latest.csv")
raw_hist   <- paste0(base_raw, "implied_fp_history.csv")

props_latest <- read_csv(raw_latest, show_col_types = FALSE) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))

props_hist <- read_csv(raw_hist, show_col_types = FALSE) %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))


# --- Compute 1-day movement from history (implied FP) ---
trend_1d <- {
  if (nrow(props_hist) == 0 || length(unique(props_hist$date)) < 2) {
    tibble(player = character(), delta_1d = numeric(), trend = character())
  } else {
    last_two <- sort(unique(props_hist$date), decreasing = TRUE)[1:2]
    latest_date <- last_two[1]
    prev_date   <- last_two[2]
    
    latest <- props_hist %>%
      filter(date == latest_date) %>%
      select(player, latest_fp = implied_fantasy_points)
    
    prev <- props_hist %>%
      filter(date == prev_date) %>%
      select(player, prev_fp = implied_fantasy_points)
    
    latest %>%
      left_join(prev, by = "player") %>%
      mutate(
        delta_1d = latest_fp - prev_fp,
        trend = case_when(
          is.na(delta_1d) ~ "•",
          delta_1d > 0    ~ "▲",
          delta_1d < 0    ~ "▼",
          TRUE            ~ "•"
        )
      ) %>%
      select(player, delta_1d, trend)
  }
}

# --- UI ---
ui <- navbarPage(
  title = "2025 Vegas Driven Fantasy Projections",
  tabPanel(
    "Rankings",
    fluidPage(
      p("Made by ",
        a("Pranav Rajaram", href = "https://twitter.com/_pranavrajaram", target = "blank")),
      sidebarLayout(
        sidebarPanel(
          selectInput("position", "Choose a Position", choices = unique(props_latest$position))
        ),
        mainPanel(
          gt_output("table")
        )
      )
    )
  ),
  tabPanel(
    "Player",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectizeInput(
            "player_sel",
            "Select Player",
            choices = sort(unique(props_hist$player)),
            options = list(placeholder = "Start typing a name"),
            selected = NULL
          ),
          checkboxGroupInput(
            "player_metrics",
            "Metrics",
            choices = c(
              "Implied Fantasy Points" = "implied_fantasy_points",
              "Pass Yds (Exp)" = "pass_yds_expected",
              "Pass TDs (Exp)" = "pass_tds_expected",
              "Rush Yds (Exp)" = "rush_yds_expected",
              "Rush TDs (Exp)" = "rush_tds_expected",
              "Rec Yds (Exp)"  = "rec_yds_expected",
              "Rec TDs (Exp)"  = "rec_tds_expected"
            ),
            selected = c("implied_fantasy_points", "rec_yds_expected", "rush_yds_expected", "pass_yds_expected",
                         "rec_tds_expected", "rush_tds_expected", "pass_tds_expected")
          ),
          helpText("Shows daily movement (by snapshot date).")
        ),
        mainPanel(
          gt_output("player_table"),
          br(),
          gt_output("player_lines_table")
        )
      )
    )
  ),
  tabPanel(
    "Movement",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("days_back", "Lookback (days):", min = 2, max = 30, value = 2, step = 1),
          selectizeInput(
            "move_positions",
            "Positions",
            choices = c("QB","RB","WR","TE"),
            multiple = TRUE,
            selected = c("QB","RB","WR","TE")
          ),
          numericInput("top_n", "Show Top N (by |Implied FP Δ|):", value = 100, min = 5, max = 100, step = 5),
          helpText("Compares the earliest vs. latest snapshot in the window for all stats.")
        ),
        mainPanel(
          gt_output("movement_table")
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # ------------- Rankings -------------
  selectedData <- reactive({
    df <- props_latest %>%
      filter(position == input$position) %>%
      arrange(desc(implied_fantasy_points)) %>%
      left_join(trend_1d, by = "player")  # add 1-day trend
    
    if (input$position == "QB") {
      df <- df %>%
        select(headshot_url, player, team, position,
               pass_yds_expected, pass_tds_expected,
               rush_yds_expected, rush_tds_expected,
               implied_fantasy_points, delta_1d, trend)
    } else if (input$position == "RB") {
      df <- df %>%
        select(headshot_url, player, team, position,
               rec_yds_expected, rec_tds_expected,
               rush_yds_expected, rush_tds_expected,
               implied_fantasy_points, delta_1d, trend)
    } else if (input$position %in% c("WR", "TE")) {
      df <- df %>%
        select(headshot_url, player, team, position,
               rec_yds_expected, rec_tds_expected,
               implied_fantasy_points, delta_1d, trend)
    }
    
    df
  })
  
  output$table <- render_gt({
    df <- selectedData()
    req(nrow(df) > 0)
    
    col_labels <- list(
      headshot_url = "",
      player = "Player",
      team = "Team",
      position = "Pos",
      pass_yds_expected = "Pass YDs (Exp)",
      pass_tds_expected = "Pass TDs (Exp)",
      rush_yds_expected = "Rush YDs (Exp)",
      rush_tds_expected = "Rush TDs (Exp)",
      rec_yds_expected = "Rec YDs (Exp)",
      rec_tds_expected = "Rec TDs (Exp)",
      implied_fantasy_points = "Fantasy Pts",
      delta_1d = "Δ 1d (FP)",
      trend = ""
    )
    
    df %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(headshot_url)),
        fn = function(x) web_image(url = x, height = 30)
      ) %>%
      cols_label(.list = col_labels[names(df)]) %>%
      fmt_number(columns = where(is.numeric), decimals = 1) %>%
      tab_header(title = paste("Vegas Driven", input$position, "Projections")) %>%
      gt_color_rows(columns = c(implied_fantasy_points, delta_1d), palette = 'ggsci::blue_material') %>%
      gt_theme_espn()
  })
  
  # ------------- Player tab -------------
  player_hist <- reactive({
    req(nrow(props_hist) > 0, input$player_sel)
    props_hist %>%
      filter(player == input$player_sel) %>%
      arrange(desc(date))
      
  })
  
  .allowed_metrics_by_pos <- function(pos) {
    if (pos == "QB") {
      c("implied_fantasy_points",
        "pass_yds_expected","pass_tds_expected",
        "rush_yds_expected","rush_tds_expected",
        "rec_yds_expected","rec_tds_expected")
    } else if (pos == "RB") {
      c("implied_fantasy_points",
        "rush_yds_expected","rush_tds_expected",
        "rec_yds_expected","rec_tds_expected")
    } else { # WR / TE
      c("implied_fantasy_points",
        "rec_yds_expected","rec_tds_expected")
    }
  }
  
  # ------------- Player tab (position-aware) -------------
  output$player_table <- render_gt({
    df <- player_hist()
    req(nrow(df) > 0)
    
    pos <- df$position[1]
    allowed <- .allowed_metrics_by_pos(pos)
    
    # intersect user's selections with allowed, always keep implied FP
    metrics <- intersect(input$player_metrics, allowed)
    if (!"implied_fantasy_points" %in% metrics)
      metrics <- c("implied_fantasy_points", metrics)
    
    show_cols <- unique(c("date", "team", "position", metrics))
    
    # Δ vs previous date for selected metrics
    df_delta <- df %>%
      arrange(desc(date)) %>%
      mutate(across(all_of(metrics), ~ . - dplyr::lag(.), .names = "{.col}_chg"))
    
    out <- df_delta %>%
      select(any_of(show_cols), ends_with("_chg"))
    
    out %>%
      gt() %>%
      cols_label(.list = setNames(
        nm = names(out),
        object = names(out) %>%
          stringr::str_replace("_expected", " (Exp)") %>%
          stringr::str_replace("_chg$", " Δ")
      )) %>%
      fmt_number(columns = where(is.numeric), decimals = 2) %>%
      tab_header(title = paste(df$player[1], "— Daily Movement")) %>%
      gt_theme_espn()
  })
  
  output$player_lines_table <- render_gt({
    df <- player_hist()
    req(nrow(df) > 0)
    
    pos <- df$position[1]
    
    # start from all *_line and _{over,under}_price columns
    raw_cols <- names(df)[str_detect(names(df), "_line$|_(over|under)_price$")]
    
    # filter by position
    keep_cols <- raw_cols
    if (pos != "QB") {
      keep_cols <- keep_cols[!str_detect(keep_cols, "^pass_")]
    }
    if (pos %in% c("WR","TE")) {
      keep_cols <- keep_cols[!str_detect(keep_cols, "^rush_")]
    }
    
    if (length(keep_cols) == 0) {
      return(
        tibble(Note = "No lines available for this position.") %>%
          gt() %>%
          tab_header(title = "Lines & Prices (Daily)") %>%
          gt_theme_espn()
      )
    }
    
    df %>%
      select(date, any_of(keep_cols)) %>%
      gt() %>%
      fmt_number(columns = where(is.numeric), decimals = 2) %>%
      tab_header(title = "Lines & Prices (Daily)") %>%
      gt_theme_espn()
  })
  
  # ------------- Movement tab (all stats: start vs end) -------------
  output$movement_table <- render_gt({
    req(nrow(props_hist) > 0)
    
    # which stats to compare
    movement_metrics <- c(
      "implied_fantasy_points",
      "pass_yds_expected", "pass_tds_expected",
      "rush_yds_expected", "rush_tds_expected",
      "rec_yds_expected",  "rec_tds_expected"
    )
    
    pos <- input$move_positions
    days_back <- input$days_back
    
    # restrict to positions & date window
    max_date <- max(props_hist$date, na.rm = TRUE)
    min_date <- max_date - days(days_back - 1)
    
    window <- props_hist %>%
      filter(position %in% pos, date >= min_date, date <= max_date)
    
    req(nrow(window) > 0)
    
    # earliest vs latest within window for each player
    earliest <- window %>%
      group_by(player, team, position, headshot_url) %>%
      slice_min(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player, team, position, headshot_url, earliest_date = date, all_of(movement_metrics)) %>%
      rename_with(~ paste0(.x, "_from"), all_of(movement_metrics))
    
    latest <- window %>%
      group_by(player, team, position, headshot_url) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(player, team, position, headshot_url, latest_date = date, all_of(movement_metrics)) %>%
      rename_with(~ paste0(.x, "_to"), all_of(movement_metrics))
    
    joined <- earliest %>%
      inner_join(latest, by = c("player","team","position","headshot_url"))
    
    # create Δ columns for every metric
    for (m in movement_metrics) {
      joined[[paste0(m, "_delta")]] <- joined[[paste0(m, "_to")]] - joined[[paste0(m, "_from")]]
    }
    
    # rank by |Δ Implied FP|
    joined <- joined %>%
      mutate(abs_fp_change = abs(`implied_fantasy_points_delta`)) %>%
      arrange(desc(abs_fp_change))
    
    # Top N
    topn <- min(input$top_n, nrow(joined))
    out <- joined %>%
      slice_head(n = topn) %>%
      select(
        headshot_url, player, team, position,
        earliest_date, latest_date,
        # show from/to/Δ for each metric
        !!!syms(as.vector(rbind(
          paste0(movement_metrics, "_from"),
          paste0(movement_metrics, "_to"),
          paste0(movement_metrics, "_delta")
        )))
      )
    
    # Labels
    pretty_name <- function(x) {
      x %>%
        str_replace("^implied_fantasy_points", "Implied FP") %>%
        str_replace("^pass_yds_expected", "Pass Yds (Exp)") %>%
        str_replace("^pass_tds_expected", "Pass TDs (Exp)") %>%
        str_replace("^rush_yds_expected", "Rush Yds (Exp)") %>%
        str_replace("^rush_tds_expected", "Rush TDs (Exp)") %>%
        str_replace("^rec_yds_expected",  "Rec Yds (Exp)") %>%
        str_replace("^rec_tds_expected",  "Rec TDs (Exp)") %>%
        str_replace("_from$", " — From") %>%
        str_replace("_to$",   " — To") %>%
        str_replace("_delta$", " Δ")
    }
    
    labels <- setNames(lapply(names(out), pretty_name), names(out))
    
    out %>%
      # keep only rows with at least one non-zero Δ
      filter(implied_fantasy_points_delta != 0) %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(headshot_url)),
        fn = function(x) web_image(url = x, height = 24)
      ) %>%
      cols_label(.list = c(labels, headshot_url = "")) %>%
      fmt_date(columns = c(earliest_date, latest_date), date_style = 3) %>%
      fmt_number(columns = where(is.numeric), decimals = 2) %>%
      tab_header(
        title = paste0(
          "Movement (Earliest vs. Latest in Last ",
          days_back, " Day", ifelse(days_back == 1, "", "s"),
          ")"
        )
      ) %>%
      gt_theme_espn()
    
  })
}

shinyApp(ui = ui, server = server)
