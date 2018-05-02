library(shiny)
library(tidyverse)
library(tidycensus)
library(ggplot2)
nytime_api_key<-"09b24102ec2b427f8e54b763b2fb4763"
census_api_key<-"1845b8c01c0825da9d7156fd6d788370bd42b7ca"

ui <- fluidPage(
  titlePanel("American Community Survey"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Create plots with 
               information from the US Census."),
      
      selectInput("State", "State",
                  choices = state.abb,
                  selected = "NJ"),
      radioButtons("Type", "Type",
                   choices = list("median gross rent",
                                  "median household income",
                                  "ratio"), 
                   selected = "ratio")
      ),
    
    
    mainPanel(plotOutput("Plot"))
  )
  )

server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(median_gross_rent = "B25064_001" , median_household_income = "B19013_001"),
      state = input$State,
      geometry = TRUE
    ) %>% .[, -5] %>% data.frame() %>% 
      
      spread(key = variable, value = estimate) %>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  
  output$Plot <- renderPlot({
    
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type)) + geom_sf() + ggtitle(input$Type) + 
      scale_fill_gradientn(colours = rainbow(7))
  })
  
}
shinyApp(ui = ui, server = server)