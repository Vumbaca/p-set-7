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

midterms <- read_rds("shiny_data.rds")

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
     if(input$demo == "Age"){
       ggplot(midterms, aes(x = p_18, y = perc_error)) +
         geom_point(color = "blue") +
         geom_smooth(color = "blue") +
         ylim(-10, 10)
       # ggplot(midterms, aes(x = p_30, y = perc_error)) +
       #   geom_point(color = "red") +
       #   geom_smooth(color = "red") +
       #   ylim(-10, 10)
       # ggplot(midterms, aes(x = p_45, y = perc_error)) +
       #   geom_point(color = "green") +
       #   geom_smooth(color = "green") +
       #   ylim(-10, 10)
       # ggplot(midterms, aes(x = p_65, y = perc_error)) +
       #   geom_point(color = "purple") +
       #   geom_smooth(color = "purple") +
       #   ylim(-10, 10)
       # ggtitle("Title") xlab("X") ylab("Y")
     }
     else{
       if(input$demo == "Education"){
         ggplot(midterms, aes(x = p_high, y = perc_error)) +
           geom_point(color = "blue") +
           geom_smooth(color = "blue") +
           ylim(-10, 10)
         ggplot(midterms, aes(x = p_some, y = perc_error)) +
           geom_point(color = "red") +
           geom_smooth(color = "red") +
           ylim(-10, 10)
         ggplot(midterms, aes(x = p_coll, y = perc_error)) +
           geom_point(color = "green") +
           geom_smooth(color = "green") +
           ylim(-10, 10)
         ggplot(midterms, aes(x = p_post, y = perc_error)) +
           geom_point(color = "purple") +
           geom_smooth(color = "purple") +
           ylim(-10, 10)
       }
       else{
          if(input$demo == "Race"){
           ggplot(midterms, aes(x = p_white, y = perc_error)) +
             geom_point(color = "blue") +
             geom_smooth(color = "blue") +
             ylim(-10, 10)
           ggplot(midterms, aes(x = p_hispanic, y = perc_error)) +
             geom_point(color = "red") +
             geom_smooth(color = "red") +
             ylim(-10, 10)
           ggplot(midterms, aes(x = p_black, y = perc_error)) +
             geom_point(color = "green") +
             geom_smooth(color = "green") +
             ylim(-10, 10)
           ggplot(midterms, aes(x = p_asian, y = perc_error)) +
             geom_point(color = "purple") +
             geom_smooth(color = "purple") +
             ylim(-10, 10)
          }
          else{
            if(input$demo == "Gender"){
             ggplot(midterms, aes(x = p_male, y = perc_error)) +
               geom_point(color = "blue") +
               geom_smooth(color = "blue") +
               ylim(-10, 10)
             ggplot(midterms, aes(x = p_female, y = perc_error)) +
               geom_point(color = "red") +
               geom_smooth(color = "red") +
               ylim(-10, 10)
            }
            else{
             if(input$demo == "Party"){
               ggplot(midterms, aes(x = p_dem, y = perc_error)) +
                 geom_point(color = "blue") +
                 geom_smooth(color = "blue") +
                 ylim(-10, 10)
               ggplot(midterms, aes(x = p_rep, y = perc_error)) +
                 geom_point(color = "red") +
                 geom_smooth(color = "red") +
                 ylim(-10, 10)
               ggplot(midterms, aes(x = p_indep, y = perc_error)) +
                 geom_point(color = "green") +
                 geom_smooth(color = "green") +
                 ylim(-10, 10)
             }
}}}}
   })
}

shinyApp(ui = ui, server = server)