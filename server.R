library(shiny)
library(nufflytics)
library(tidyverse)

O_divs <- c("Season 8 Div 1", "Season 8 Div 2", "Season 8 Div 3", "Season 8 Div 4A", "Season 8 Div 4B")
G_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6A", "Season 8 - Division 6B", "Season 8 - Division 6C", "Season 8 - Division 6D", "Season 8 - Division 6E")
R_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6", "Season 8 - Division 7", "Season 8 - Division 8", "Season 8 - Division 9A", "Season 8 - Division 9B", "Season 8 - Division 9C", "Season 8 - Division 9D", "Season 8 - Division 9E" )

divisions = list("REBBL - Big O" = O_divs, "REBBL - Gman" = G_divs, "REBBL - REL" = R_divs) %>% 
    purrr::map(~magrittr::set_names(., stringr::str_replace_all(., "Season 8 (- )?Div(ision)?","Div")))

key <- readRDS("data/api.key")

shinyServer(function(input, output, session) {
    
    observeEvent(input$league, {
        validate(need(input$league, message = F)) 
        updateSelectizeInput(session, "division", choices = c("Division:" = "", divisions[[input$league]]))
    })
    
    ladder <- eventReactive(input$division,
                 {
                     validate(need(input$league, message=F), need(input$division, message = F))
                     teams <- withProgress(nufflytics::api_teams(key, league = input$league, competition = input$division), value = 1, message = "Loading teams...")
                     
                     team_choices <- teams$teams$id %>% magrittr::set_names(teams$teams$team)
                     
                     updateSelectizeInput(session, "home_team", choices = c("Home Team:"="", team_choices))
                     updateSelectizeInput(session, "away_team", choices = c("Away Team:"="", team_choices))
                     withProgress(ladder <- nufflytics::api_ladder(key, league = input$league, competition = input$division), value = 1, message = "Loading ladder...")
                     
                     ladder <- ladder$ranking %>%
                         flatten() %>% 
                         as.data.frame() %>% 
                         separate(w.d.l, c("W","D","L")) %>% 
                         mutate(Team=name,Race = map_chr(race, id_to_race), Coach=name.1,Pts=score, TV=tv) %>% 
                         select(Team,Race,TV,Coach,W,D,L,Pts) %>% 
                         mutate_at(vars("W","D","L"), as.integer)
                    return(ladder)
                }
    )
    
    
   
    
    team_listener <- reactive(list(input$home_team, input$away_team))
    teams <- eventReactive(team_listener(),{
        if(input$home_team != "" & input$away_team != "") {
            withProgress({
                setProgress(value = 0.5, message = "Loading Home Team...")
                h_team <- api_team(key, id = input$home_team)
                setProgress(value = 1, message = "Loading Away Team...")
                a_team <- api_team(key, id = input$away_team)
            })
            return(list(home = h_team, away = a_team))
        }
    })
    
    observeEvent(input$show_ladder,{
        data <- list(
            content = ladder(),
            type = "table",
            header = "Division Standings"
        )
        
        saveRDS(data, file = "data/infobox.rds")
        saveRDS("show", file = "data/action.rds")
    })
    
    observeEvent(input$hide_display, saveRDS("hide", file = "data/action.rds"))
    
    output$debug <- renderTable({
        validate(need(ladder(), message = F), need(input$home_team, message = F), need(input$away_team, message = F))
        ladder()
        })
    output$debug2 <- renderText(teams()$home$team$name)
})