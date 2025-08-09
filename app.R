library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(gt)
library(gtExtras)

REPO_OWNER <- "pranavrajaram"
REPO_NAME  <- "vegas-fantasy"

load_latest_data <- function() {
  url <- paste0(
    "https://raw.githubusercontent.com/",
    REPO_OWNER, "/", REPO_NAME,
    "/main/data/implied_fp_latest.csv"
  )
  read_csv(url, show_col_types = FALSE)
}

props_data <- load_latest_data() %>%
  filter(position %in% c("QB", "RB", "WR", "TE"))
ui <- fluidPage(
  titlePanel("2025 Vegas Driven Fantasy Projections"),
  p("Made by ",
    a("Pranav Rajaram", href = "https://twitter.com/_pranavrajaram", target = "blank")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("position", "Choose a Position", choices = unique(props_data$position))
    ),
    mainPanel(
      gt_output("table")
    )
  )
)

server <- function(input, output, session) {
  
  selectedData <- reactive({
    df <- props_data %>%
      filter(position == input$position) %>%
      arrange(desc(implied_fantasy_points))
    
    # Select columns based on position
    if (input$position == "QB") {
      df <- df %>%
        select(headshot_url, player, team, position,
               pass_yds_expected, pass_tds_expected,
               rush_yds_expected, rush_tds_expected,
               implied_fantasy_points)
    } else if (input$position == "RB") {
      df <- df %>%
        select(headshot_url, player, team, position,
               rec_yds_expected, rec_tds_expected,
               rush_yds_expected, rush_tds_expected,
               implied_fantasy_points)
    } else if (input$position %in% c("WR", "TE")) {
      df <- df %>%
        select(headshot_url, player, team, position,
               rec_yds_expected, rec_tds_expected,
               implied_fantasy_points)
    }
    
    df
  })
  
  output$table <- render_gt({
    df <- selectedData()
    
    # Apply labels dynamically
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
      implied_fantasy_points = "Fantasy Pts"
    )
    
    df %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(headshot_url)),
        fn = function(x) web_image(url = x, height = 30)
      ) %>%
      cols_label(.list = col_labels[names(df)]) %>%  # only label columns that exist
      fmt_number(
        columns = where(is.numeric),
        decimals = 1
      ) %>%
      tab_header(
        title = paste("Vegas Driven ", input$position, " Projections ")
      ) %>%
      gt_color_rows(
        columns = c(implied_fantasy_points),
        palette = 'ggsci::blue_material'
      ) %>% 
      gt_theme_espn()
  })
}

shinyApp(ui = ui, server = server)
