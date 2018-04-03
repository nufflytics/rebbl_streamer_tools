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
        column(2, actionButton("hide_display", "Remove display", class = "btn-danger")),
        column(2, actionButton("show_ladder", "Show Ladder"))
        ),
    tableOutput("debug"),
    verbatimTextOutput("debug2")
    
)