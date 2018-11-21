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
        tabsetPanel(type = "tabs",
                    tabPanel("1", plotOutput("plot1")),
                    tabPanel("2", plotOutput("plot2")),
                    tabPanel("3", plotOutput("plot3")),
                    tabPanel("4", plotOutput("plot4"))
        ),
        
        dataTableOutput("table")
      )
   )
)

server <- function(input, output) {
   
   output$plot1 <- renderPlot({
     if(input$demo == "Age"){
       ggplot(midterms, aes(x = p_18, y = error)) +
         geom_point(color = "blue") +
         geom_smooth(color = "blue") +
         ylim(-15, 15)
       # ggtitle("Title") xlab("X") ylab("Y")
     }
     else{
       if(input$demo == "Education"){
         ggplot(midterms, aes(x = p_high, y = error)) +
           geom_point(color = "blue") +
           geom_smooth(color = "blue") +
           ylim(-15, 15)
       }
       else{
          if(input$demo == "Race"){
           ggplot(midterms, aes(x = p_white, y = error)) +
             geom_point(color = "blue") +
             geom_smooth(color = "blue") +
             ylim(-15, 15)
          }
          else{
            if(input$demo == "Gender"){
             ggplot(midterms, aes(x = p_male, y = error)) +
               geom_point(color = "blue") +
               geom_smooth(color = "blue") +
               ylim(-15, 15)
            }
            else{
             if(input$demo == "Party"){
               ggplot(midterms, aes(x = p_dem, y = error)) +
                 geom_point(color = "blue") +
                 geom_smooth(color = "blue") +
                 ylim(-15, 15)
             }
            }
          }
       }
     }
   })
   
   output$plot2 <- renderPlot({
     if(input$demo == "Age"){
       ggplot(midterms, aes(x = p_30, y = error)) +
         geom_point(color = "red") +
         geom_smooth(color = "red") +
         ylim(-15, 15)
       # ggtitle("Title") xlab("X") ylab("Y")
     }
     else{
       if(input$demo == "Education"){
         ggplot(midterms, aes(x = p_some, y = error)) +
           geom_point(color = "red") +
           geom_smooth(color = "red") +
           ylim(-15, 15)
       }
       else{
         if(input$demo == "Race"){
           ggplot(midterms, aes(x = p_hispanic, y = error)) +
             geom_point(color = "red") +
             geom_smooth(color = "red") +
             ylim(-15, 15)
         }
         else{
           if(input$demo == "Gender"){
             ggplot(midterms, aes(x = p_female, y = error)) +
               geom_point(color = "red") +
               geom_smooth(color = "red") +
               ylim(-15, 15)
           }
           else{
             if(input$demo == "Party"){
               ggplot(midterms, aes(x = p_rep, y = error)) +
                 geom_point(color = "red") +
                 geom_smooth(color = "red") +
                 ylim(-15, 15)
             }
           }
         }
       }
     }
   })
   
   output$plot3 <- renderPlot({
     if(input$demo == "Age"){
       ggplot(midterms, aes(x = p_45, y = error)) +
         geom_point(color = "purple") +
         geom_smooth(color = "purple") +
         ylim(-15, 15)
       # ggtitle("Title") xlab("X") ylab("Y")
     }
     else{
       if(input$demo == "Education"){
         ggplot(midterms, aes(x = p_coll, y = error)) +
           geom_point(color = "purple") +
           geom_smooth(color = "purple") +
           ylim(-15, 15)
       }
       else{
         if(input$demo == "Race"){
           ggplot(midterms, aes(x = p_black, y = error)) +
             geom_point(color = "purple") +
             geom_smooth(color = "purple") +
             ylim(-15, 15)
         }
         else{
           if(input$demo == "Party"){
            ggplot(midterms, aes(x = p_indep, y = error)) +
              geom_point(color = "purple") +
              geom_smooth(color = "purple") +
              ylim(-15, 15)
           }
           else{}
         }
       }
     }
   })
   
   output$plot4 <- renderPlot({
     if(input$demo == "Age"){
       ggplot(midterms, aes(x = p_65, y = error)) +
         geom_point(color = "green") +
         geom_smooth(color = "green") +
         ylim(-15, 15)
       # ggtitle("Title") xlab("X") ylab("Y")
     }
     else{
       if(input$demo == "Education"){
         ggplot(midterms, aes(x = p_post, y = error)) +
           geom_point(color = "green") +
           geom_smooth(color = "green") +
           ylim(-15, 15)
       }
       else{
         if(input$demo == "Race"){
           ggplot(midterms, aes(x = p_asian, y = error)) +
             geom_point(color = "green") +
             geom_smooth(color = "green") +
             ylim(-15, 15)
         }
         else{
           if(input$demo == "Party"){
             ggplot(midterms, aes(x = p_other, y = error)) +
               geom_point(color = "green") +
               geom_smooth(color = "green") +
               ylim(-15, 15)
           }
           else{}
         }
       }
     }
   })
}

shinyApp(ui = ui, server = server)