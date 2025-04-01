library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# UI Part
ui <- dashboardPage(
  dashboardHeader(title = "IPL Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Team Analysis", tabName = "team", icon = icon("users")),
      menuItem("Match Analysis", tabName = "match", icon = icon("trophy"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_matches", width = 4),
                valueBoxOutput("total_teams", width = 4),
                valueBoxOutput("total_seasons", width = 4)
              ),
              fluidRow(
                box(plotOutput("matches_by_season"), width = 6,
                    title = "Matches per Season"),
                box(plotOutput("toss_decisions"), width = 6,
                    title = "Toss Decisions")
              )
      ),
      
      # Team Analysis Tab
      tabItem(tabName = "team",
              fluidRow(
                box(plotOutput("team_wins"), width = 12,
                    title = "Team Performance")
              ),
              fluidRow(
                box(plotOutput("win_by_venue"), width = 12,
                    title = "Wins by Venue (Top 10)")
              )
      ),
      
      # Match Analysis Tab
      tabItem(tabName = "match",
              fluidRow(
                box(plotOutput("win_by_runs"), width = 6,
                    title = "Distribution of Wins by Runs"),
                box(plotOutput("win_by_wickets"), width = 6,
                    title = "Distribution of Wins by Wickets")
              )
      )
    )
  )
)

# Server Part
server <- function(input, output) {
  # Read data
  ipl_data <- read.csv("C:/Users/salun/OneDrive/Desktop/R_dashboard/Data/IPL_Dataset(2008-2024).csv")
  
  # Value Boxes
  output$total_matches <- renderValueBox({
    valueBox(
      nrow(ipl_data), "Total Matches",
      icon = icon("cricket-ball"),
      color = "red"
    )
  })
  
  output$total_teams <- renderValueBox({
    valueBox(
      length(unique(c(ipl_data$Team1, ipl_data$Team2))), "Teams",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$total_seasons <- renderValueBox({
    valueBox(
      length(unique(ipl_data$Season)), "Seasons",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  # Matches by Season Plot
  output$matches_by_season <- renderPlot({
    ipl_data %>%
      count(Date) %>%
      ggplot(aes(x = as.factor(Date), y = n)) +
      geom_bar(stat = "identity", fill = "yellow") +
      theme_minimal() +
      labs(x = "Season", y = "Number of Matches") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Toss Decisions Plot
  output$toss_decisions <- renderPlot({
    ipl_data %>%
      count(Toss_Decision) %>%
      ggplot(aes(x = "", y = n , fill = Toss_Decision)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_minimal() +
      labs(fill = "Toss Decision")
  })
  
  # Team Wins Plot
  output$team_wins <- renderPlot({
    ipl_data %>%
      count(match_Winner) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(match_Winner, n), y = n)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Team", y = "Number of Wins")
  })
  
  # Wins by Venue Plot
  output$win_by_venue <- renderPlot({
    ipl_data %>%
      count(Venue) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(Venue, n), y = n)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Venue", y = "Number of Matches")
  })
  
  # Win by Runs Distribution
  output$win_by_runs <- renderPlot({
    ipl_data %>%
      filter(Win_Type == "runs") %>%
      ggplot(aes(x = Win_Margin)) +
      geom_histogram(fill = "green", bins = 30) +
      theme_minimal() +
      labs(x = "Runs", y = "Frequency")
  })
  
  # Win by Wickets Distribution
  output$win_by_wickets <- renderPlot({
    ipl_data %>%
      filter(Win_Type == "wickets") %>%
      ggplot(aes(x = Win_Margin)) +
      geom_histogram(fill = "coral", bins = 10) +
      theme_minimal() +
      labs(x = "Wickets", y = "Frequency")
  })
}

# Run the app
shinyApp(ui = ui, server = server)