library(shiny)

library(tidyverse)

# read data from file created by scratch to new dataframe
midterms <- read_rds("shiny_data.rds")

# set up app ui
ui <- fluidPage(
   
   # add title
   titlePanel("Polling Errors"),
   
   # organize layout
   sidebarLayout(
     
      # describe input
      sidebarPanel(
        
        # solicit input regarding demographic of interest
        selectInput("demo", "Choose Demographic Variable", c("Age", "Education", "Race", "Gender", "Party"), selected = "Age")
        
      ),
      
      #describe output
      mainPanel(
        
        # create tabs for plots
        tabsetPanel(type = "tabs",
                    # create first plot tab
                    tabPanel("1", plotOutput("plot1")),
                    # create second plot tab
                    tabPanel("2", plotOutput("plot2")),
                    # create third plot tab
                    tabPanel("3", plotOutput("plot3")),
                    # create fourth plot tab
                    tabPanel("4", plotOutput("plot4"))
                    
        ),
        
        # create table below plots
        dataTableOutput("table")
        
      )
      
   )
   
)

# create server for app
server <- function(input, output) {
   
   # describe first plot output
   output$plot1 <- renderPlot({
     
     # describe output if age was selected followed by what to do if age was not selected
     if(input$demo == "Age"){
       
       # plot 
       ggplot(midterms, aes(x = p_18, y = error)) +
         
         # make points blue
         geom_point(color = "blue") +
         
         # add blue line of best fit
         geom_smooth(color = "blue") +
         
         # center y axis on zero with limits at positive and negative fifteen
         ylim(-15, 15) +
         
         # title plot
         ggtitle("Error and 18 to 29 Year Old Voters") +
         
         # label plot x axis
         xlab("Percent in Age Range") +
         
         # label plot y axis
         ylab("Republican Advantage Error [Actual - Predicted]")
       
     }
     
     else{
       
       # repeat age procedure for education plot
       if(input$demo == "Education"){
         
         ggplot(midterms, aes(x = p_high, y = error)) +
           
           geom_point(color = "blue") +
           
           geom_smooth(color = "blue") +
           
           ylim(-15, 15) +
           
           ggtitle("Error and High School Graduate and Below Voters") +
           
           xlab("Percent at Education Level") +
           
           ylab("Republican Advantage Error [Actual - Predicted]")
         
       }
       
       else{
         
          # repeat age procedure for race plot
          if(input$demo == "Race"){
            
           ggplot(midterms, aes(x = p_white, y = error)) +
              
             geom_point(color = "blue") +
              
             geom_smooth(color = "blue") +
              
             ylim(-15, 15) +
              
             ggtitle("Error and White Voters") +
              
             xlab("Percent of Given Race") +
              
             ylab("Republican Advantage Error [Actual - Predicted]") 
            
          }
         
          else{
            
            # repeat age procedure for gender plot
            if(input$demo == "Gender"){
              
             ggplot(midterms, aes(x = p_male, y = error)) +
                
               geom_point(color = "blue") +
                
               geom_smooth(color = "blue") +
                
               ylim(-15, 15) +
                
               ggtitle("Error and Male Voters") +
                
               xlab("Percent in Given Gender") +
                
               ylab("Republican Advantage Error [Actual - Predicted]")
              
            }
            
            else{
              
             # repeat age procedure for party plot
             if(input$demo == "Party"){
               
               ggplot(midterms, aes(x = p_dem, y = error)) +
                 
                 geom_point(color = "blue") +
                 
                 geom_smooth(color = "blue") +
                 
                 ylim(-15, 15) +
                 
                 ggtitle("Error and Democratic Voters") +
                 
                 xlab("Percent in Political Party") +
                 
                 ylab("Republican Advantage Error [Actual - Predicted]")
               
             }
              
            }
            
          }
         
       }
       
     }
     
   })
   
   # repeat plot1 procedure with next option for each demographic and red coloring
   output$plot2 <- renderPlot({
     
     if(input$demo == "Age"){
       
       ggplot(midterms, aes(x = p_30, y = error)) +
         
         geom_point(color = "red") +
         
         geom_smooth(color = "red") +
         
         ylim(-15, 15) +
         
         ggtitle("Error and 30 to 44 Year Old Voters") +
         
         xlab("Percent in Age Range") +
         
         ylab("Republican Advantage Error [Actual - Predicted]")

     }
     
     else{
       
       if(input$demo == "Education"){
         
         ggplot(midterms, aes(x = p_some, y = error)) +
           
           geom_point(color = "red") +
           
           geom_smooth(color = "red") +
           
           ylim(-15, 15) +
           
           ggtitle("Error and Some College Attendee Voters") +
           
           xlab("Percent in Education Level") +
           
           ylab("Republican Advantage Error [Actual - Predicted]")
         
       }
       
       else{
         
         if(input$demo == "Race"){
           
           ggplot(midterms, aes(x = p_hispanic, y = error)) +
             
             geom_point(color = "red") +
             
             geom_smooth(color = "red") +
             
             ylim(-15, 15) +
             
             ggtitle("Error and Hispanic Voters") +
             
             xlab("Percent in Given Race") +
             
             ylab("Republican Advantage Error [Actual - Predicted]")
           
         }
         
         else{
           
           if(input$demo == "Gender"){
             
             ggplot(midterms, aes(x = p_female, y = error)) +
               
               geom_point(color = "red") +
               
               geom_smooth(color = "red") +
               
               ylim(-15, 15) +
               
               ggtitle("Error and Female Voters") +
               
               xlab("Percent in Given Gender") +
               
               ylab("Republican Advantage Error [Actual - Predicted]")
             
           }
           
           else{
             
             if(input$demo == "Party"){
               
               ggplot(midterms, aes(x = p_rep, y = error)) +
                 
                 geom_point(color = "red") +
                 
                 geom_smooth(color = "red") +
                 
                 ylim(-15, 15) +
                 
                 ggtitle("Error and Republican Voters") +
                 
                 xlab("Percent in Political Party") +
                 
                 ylab("Republican Advantage Error [Actual - Predicted]")
               
             }
             
           }
           
         }
         
       }
       
     }
     
   })
   
   # repeat plot1 procedure with third option for each demographic and purple coloring
   output$plot3 <- renderPlot({
     
     if(input$demo == "Age"){
       
       ggplot(midterms, aes(x = p_45, y = error)) +
         
         geom_point(color = "purple") +
         
         geom_smooth(color = "purple") +
         
         ylim(-15, 15) +
         
         ggtitle("Error and 45 to 64 Year Old Voters") +
         
         xlab("Percent in Age Range") +
         
         ylab("Republican Advantage Error [Actual - Predicted]")

     }
     
     else{
       
       if(input$demo == "Education"){
         
         ggplot(midterms, aes(x = p_coll, y = error)) +
           
           geom_point(color = "purple") +
           
           geom_smooth(color = "purple") +
           
           ylim(-15, 15) +
           
           ggtitle("Error and College Graduate Voters") +
           
           xlab("Percent in Education Level") +
           
           ylab("Republican Advantage Error [Actual - Predicted]")
         
       }
       
       else{
         
         if(input$demo == "Race"){
           
           ggplot(midterms, aes(x = p_black, y = error)) +
             
             geom_point(color = "purple") +
             
             geom_smooth(color = "purple") +
             
             ylim(-15, 15) +
             
             ggtitle("Error and Black Voters") +
             
             xlab("Percent in Given Race") +
             
             ylab("Republican Advantage Error [Actual - Predicted]")
           
         }
         
         else{
           
           if(input$demo == "Party"){
             
            ggplot(midterms, aes(x = p_indep, y = error)) +
               
              geom_point(color = "purple") +
               
              geom_smooth(color = "purple") +
               
              ylim(-15, 15) +
               
              ggtitle("Error and Independent Voters") +
               
              xlab("Percent in Given Party") +
               
              ylab("Republican Advantage Error [Actual - Predicted]")
             
           }
           
           # no plot3 or plot4 for gender
           else{}
           
         }
         
       }
       
     }
     
   })
   
   # repeat plot1 procedure with last option for each demographic and green coloring
   output$plot4 <- renderPlot({
     
     if(input$demo == "Age"){
       
       ggplot(midterms, aes(x = p_65, y = error)) +
         
         geom_point(color = "green") +
         
         geom_smooth(color = "green") +
         
         ylim(-15, 15) +
         
         ggtitle("Error and 65 and Over Voters") +
         
         xlab("Percent in Age Range") +
         
         ylab("Republican Advantage Error [Actual - Predicted]")

     }
     
     else{
       
       if(input$demo == "Education"){
         
         ggplot(midterms, aes(x = p_post, y = error)) +
           
           geom_point(color = "green") +
           
           geom_smooth(color = "green") +
           
           ylim(-15, 15) +
         
           ggtitle("Error and Postgraduate Voters") +
           
           xlab("Percent in Education Level") +
           
           ylab("Republican Advantage Error [Actual - Predicted]")
           
       }
       
       else{
         
         if(input$demo == "Race"){
           
           ggplot(midterms, aes(x = p_asian, y = error)) +
             
             geom_point(color = "green") +
             
             geom_smooth(color = "green") +
             
             ylim(-15, 15) +
             
             ggtitle("Error and Asian Voters") +
             
             xlab("Percent in Given Race") +
             
             ylab("Republican Advantage Error [Actual - Predicted]")
           
         }
         else{
           
           if(input$demo == "Party"){
             
             ggplot(midterms, aes(x = p_other, y = error)) +
               
               geom_point(color = "green") +
               
               geom_smooth(color = "green") +
               
               ylim(-15, 15) +
               
               ggtitle("Error and Other Political Parties Voters") +
               
               xlab("Percent in Political Party") +
               
               ylab("Republican Advantage Error [Actual - Predicted]")
             
           }
           
           else{}
           
         }
         
       }
       
     }
     
   })
   
   # create tables for output
   output$table <- renderDataTable({
     
     # create table for if age was selected as user input followed by what to do if age was not selected
     if(input$demo == "Age"){
       
       midterms %>%
         
         # pick out and rename relevant variables
         select("District" = district_id, "Error" = error, "Actual Rep Adv" = rep_adv_act, "Estimated Rep Adv" = rep_adv_est,
                "18 to 29" = p_18, "30 to 44" = p_30, "45 to 64" = p_45, "65 and Over" = p_65) %>%
         
         # sort from highest to lowest error
         arrange(desc(Error))
       
     }
     
     else{
       
       # repeat for education table
       if(input$demo == "Education"){
         
         midterms %>%
           
           select("District" = district_id, "Error" = error, "Actual Rep Adv" = rep_adv_act, "Estimated Rep Adv" = rep_adv_est,
                  "High School" = p_high, "Some College" = p_some, "College" = p_coll, "Postgraduate" = p_post) %>%
           
           arrange(desc(Error))
         
       }
       
       else{
         
         if(input$demo == "Race"){
           
           midterms %>%
             
             select("District" = district_id, "Error" = error, "Actual Rep Adv" = rep_adv_act, "Estimated Rep Adv" = rep_adv_est,
                    "White" = p_white, "Hispanic" = p_hispanic, "Black" = p_black, "Asian" = p_asian) %>%
             
             arrange(desc(Error))
           
         }
         
         else{
           
           if(input$demo == "Party"){
             
             midterms %>%
               
               select("District" = district_id, "Error" = error, "Actual Rep Adv" = rep_adv_act, "Estimated Rep Adv" = rep_adv_est,
                      "Democratic" = p_dem, "Republican" = p_rep, "Independent" = p_indep, "Other" = p_other) %>%
               
               arrange(desc(Error))
             
           }
           
           else{
             
             if(input$demo == "Gender"){
               
               midterms %>%
                 
                 select("District" = district_id, "Error" = error, "Actual Rep Adv" = rep_adv_act, "Estimated Rep Adv" = rep_adv_est,
                        "Male" = p_male, "Female" = p_female) %>%
                 
                 arrange(desc(Error))
               
             }
             
           }
           
         }
         
       }
       
     }
     
   })
   
}

# build app
shinyApp(ui = ui, server = server)