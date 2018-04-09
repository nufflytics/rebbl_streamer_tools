library(shiny)
library(nufflytics)
library(tidyverse)

O_divs <- c("Season 8 Div 1", "Season 8 Div 2", "Season 8 Div 3", "Season 8 Div 4A", "Season 8 Div 4B")
G_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6A", "Season 8 - Division 6B", "Season 8 - Division 6C", "Season 8 - Division 6D", "Season 8 - Division 6E")
R_divs <- c("Season 8 - Division 1", "Season 8 - Division 2", "Season 8 - Division 3", "Season 8 - Division 4", "Season 8 - Division 5", "Season 8 - Division 6", "Season 8 - Division 7", "Season 8 - Division 8", "Season 8 - Division 9A", "Season 8 - Division 9B", "Season 8 - Division 9C", "Season 8 - Division 9D", "Season 8 - Division 9E" )

divisions = list("REBBL - Big O" = O_divs, "REBBL - Gman" = G_divs, "REBBL - REL" = R_divs) %>% 
    purrr::map(~magrittr::set_names(., stringr::str_replace_all(., "Season 8 (- )?Div(ision)?","Div")))

key <- readRDS("data/api.key")

t1 <- readRDS("data/team1.rds")
t2 <- readRDS("data/team2.rds")
l <- readRDS("data/ladder.rds")
tms <- readRDS("data/teams.rds")

convert_player_to_row <- function(player) {
    p = list(
        #id = player$id,
        name = player$name,
        type = str_replace_all(player$type, c(".*_" = "", "([a-z])([A-Z])"="\\1 \\2")),
        level = player$level,
        spp = player$xp,
        tv = player$value,
        # ma = player$attributes$ma,
        # st = player$attributes$st,
        # ag = player$attributes$ag,
        # av = player$attributes$av,
        casualties_state = glue::collapse(player$casualties_state %>% map_chr(state_to_casualty), ", "),
        skills = glue::collapse(player$skills %>% str_replace_all("([a-z])([A-Z])", "\\1 \\2"),", ")
    )
    
    if(is_empty(p$casualties_state)) p$casualties_state <- NA
    if(is_empty(p$skills)) p$skills <- NA
    
    as_data_frame(p)
}

shinyServer(function(input, output, session) {
    
    observeEvent(input$league, {
        validate(need(input$league, message = F)) 
        updateSelectizeInput(session, "division", choices = c("Division:" = "", divisions[[input$league]]))
    })
    
    ladder <- eventReactive(input$division,
                            {
                                validate(need(input$league, message=F), need(input$division, message = F))
                                #teams <- withProgress(nufflytics::api_teams(key, league = input$league, competition = input$division), value = 1, message = "Loading teams...")
                                teams = tms
                                
                                team_choices <- teams$teams$id %>% magrittr::set_names(teams$teams$team)
                                
                                updateSelectizeInput(session, "home_team", choices = c("Home Team:"="", team_choices))
                                updateSelectizeInput(session, "away_team", choices = c("Away Team:"="", team_choices))
                                
                                #withProgress(ladder <- nufflytics::api_ladder(key, league = input$league, competition = input$division), value = 1, message = "Loading ladder...")
                                ladder <- l # for testing
                                
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
                h_team <- t1 # for testing
                #h_team <- api_team(key, id = input$home_team)
                setProgress(value = 1, message = "Loading Away Team...")
                a_team <- t2 # for testing
                #a_team <- api_team(key, id = input$away_team)
            })
            return(list(home = h_team, away = a_team))
        } else return(NULL)
    })
    
    splash <- eventReactive(teams(), {
        validate(need(teams(), message = F), need(ladder(), message = F))
        
        h = teams()$home
        a = teams()$away
        
        h_record <- ladder() %>% filter(Team == h$team$name) %>% unite("wdl",W,D,L, sep = "-") %>% .$wdl
        a_record <- ladder() %>% filter(Team == a$team$name) %>% unite("wdl",W,D,L, sep = "-") %>% .$wdl
        
        tagList(
            div(class = "identifier",
                fluidRow(
                    column(12, 
                           img(class = "vs",src="http://nufflytics.com/img/main/REBBL.png", width = 250),
                           img(class = "vs",src = "http://nufflytics.com/img/main/BigO_s.png", width = 175),
                           p(class = "text-center division", input$division)
                           )
                )
                ),
            div(class = "matchup", 
                fluidRow(
                    column(class = "home_team", 5,img( class = "pull-right", height = "150px", src=glue::glue("http://images.bb2.cyanide-studio.com/logos/Logo_{h$team$logo}.png"))),
                    column(2),
                    column(class = "away_team",5)
                ),
                fluidRow(
                    column(class = "home_team", 5, div(class = "text-right", p(h$team$name), p( h$coach$name), p(h_record), p(h$team$leitmotiv))),
                    column(2, img(class = "central_col", src="img/BigIconVS.png")),
                    column(class = "away_team", 5, div(p(a$team$name), p(a$coach$name), p(a_record), p(a$team$leitmotiv)))
                ),
                fluidRow(
                    column(class = "home_team",5),
                    column(2),
                    column(class = "away_team",5, img(height = "150px", src=glue::glue("http://images.bb2.cyanide-studio.com/logos/Logo_{a$team$logo}.png")))
                )
            )
        )
    })
    
    # Buttons to send output signals ----
    observeEvent(input$show_ladder,{
        data <- list(
            content = ladder(),
            type = "table",
            header = "Division Standings"
        )
        
        saveRDS(data, file = "data/infobox.rds")
        saveRDS("show", file = "data/action.rds")
    })
    
    observeEvent(input$show_splash, {
        data <- list(
            content = splash(),
            type = "splash",
            header = ""
        )
        
        saveRDS(data, file = "data/infobox.rds")
        saveRDS("show", file = "data/action.rds")
    })
    
    observeEvent(input$hide_display, saveRDS("hide", file = "data/action.rds"))
    
    
    # Outputs for management console -----
    output$debug <- renderTable({
        validate(need(ladder(), message = F), need(input$home_team, message = F), need(input$away_team, message = F))
        ladder()
    })
    output$debug2 <- renderText(teams()$home$team$name)
    output$home_picker <- renderTable({
        validate(need(teams(), message = F), need(input$home_team, message = F))
        teams()$home$roster %>% 
            map_df(convert_player_to_row)
    })
    output$away_picker <- renderTable({
        validate(need(teams(), message = F), need(input$away_team, message = F))
        teams()$away$roster %>% 
            map_df(convert_player_to_row)
    })
})