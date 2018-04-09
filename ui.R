library(shiny)

fluidPage(
    fluidRow(
        column(3, selectizeInput("league", "", choices = c("League:" = "", "BigO" = "REBBL - Big O", "Gman" = "REBBL - Gman", "REL" = "REBBL - REL"))),
        conditionalPanel("input.league != ''", column(3, selectizeInput("division","", choices = c(""))))
    ),
    hr(),
    fluidRow(
        conditionalPanel(
            "input.division != ''",
            column(6, selectizeInput("home_team", "", choices = "")),
            column(6, selectizeInput("away_team", "", choices = "")))
    ),
    fluidRow(
        column(2, actionButton("hide_display", "Clear display", class = "btn-danger")),
        column(2, actionButton("show_splash", "Show Splash")),
        column(2, actionButton("show_ladder", "Show Ladder"))
        ),
    
    h3("Ladder"),
    tableOutput("debug"),
    h3("Home Team"),
    tableOutput("home_picker"),
    h3("Away Team"),
    tableOutput("away_picker")
    
)