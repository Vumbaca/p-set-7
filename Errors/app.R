#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

midterms <- read_rds("./Errors/shiny_data.rds")

ui <- fluidPage(
   
   titlePanel("Polling Errors"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput("demo", "Choose Demographic Variable", c("Age", "Education", "Race", "Gender", "Party"), selected = "Age")
      ),
      
      mainPanel(
         plotOutput("plot")
      )
   )
)

server <- function(input, output) {
   
   output$plot <- renderPlot({
ggplot
   })
}

shinyApp(ui = ui, server = server)