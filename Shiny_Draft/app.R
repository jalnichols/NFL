
library(shiny)
library(tidyverse)
library(extrafont)

# load data

drafted_players <- read_csv("data/drafted_players.csv")

#
#
#
#
# build Shiny app

ui <- fluidPage(
  
  titlePanel("NFL Draft (1994-2018)"),
  
  sidebarLayout(
    
    position = "right",
    
    sidebarPanel(
      
      helpText("Visualize NFL Draft results from last 25 years"),
      
      selectInput("TEAM",
                  
                  label = "Choose team to display",
                  choices = unique(drafted_players$team),
                  selected = "PIT")
      
    ),
    
    mainPanel(
      
      plotOutput("nfl_draft")
      
    )
    
  )
  
)

#

server <- function(input, output) {
  
  output$nfl_draft <- renderPlot(
    
    ggplot(data = drafted_players %>%
             filter(team == input$TEAM & draft_year < 2019), 
           aes(x = draft_year, y = round, size = relative_round, color = AV_regressed, label = player))+
      
      geom_point(size = 3, color = "black")+
      ggrepel::geom_label_repel()+
      
      labs(x = "",
           y = "Round Selected",
           title = paste0("Draft picks for ", input$TEAM))+
      scale_color_gradient2(low = "dark red", mid = "gray", high = "#37b36c", midpoint = 4)+
      scale_x_continuous(breaks = seq(1994, 2018, 1))+
      scale_y_reverse(breaks = seq(1,7,1))+
      theme(axis.text = element_text(size = 24, face = "bold"),
            axis.title = element_text(size = 24, face = "bold"),
            plot.title = element_text(size = 36, face = "bold"),
            text = element_text(family = "Roboto Condensed")),
    
    width = 1200,
    height = 600)
  
  output$nfl_draft <- renderPlot(
    
    ggplot(data = drafted_players %>%
             filter(team == input$TEAM & games > 0 & draft_year < 2019), 
           aes(x = reorder(paste0(player, "/", draft_year), AV_regressed), y = relative_round, fill = AV_regressed))+
      
      geom_bar(position = "stack", stat = "identity")+
      
      labs(x = "",
           y = "Value relative to round",
           title = paste0("Draft picks for ", input$TEAM))+
      scale_fill_gradient2(low = "dark red", mid = "gray", high = "#37b36c", midpoint = 4, name = "Season AV")+
      theme(axis.text = element_text(size = 12, face = "bold"),
            axis.title = element_text(size = 24, face = "bold"),
            plot.title = element_text(size = 36, face = "bold"),
            text = element_text(family = "Roboto Condensed"),
            legend.title = element_text(size = 16, face = "bold"))+
      coord_flip(),
    
    width = 1200,
    height = 1600
    
  )
  
  
  
}

#

shinyApp(ui, server)