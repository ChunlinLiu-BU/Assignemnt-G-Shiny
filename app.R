
library(tidyverse)
library(shiny)
library(bslib)


ui <- page_sidebar(
  
  title = "NHANES Smoking Data Explorer",
  
  sidebar = sidebar(
    navset_pill(
      id = "tabs",
      nav_panel("Pre/Post Trends"),
      nav_panel("Ever Smoker Summary"),
      nav_panel("Under 18 Smokers"),
      nav_panel("Illegal Smokers")
    )
  ),

  nav_content("Pre/Post Trends",
              sidebarLayout(
                sidebarPanel(
                  selectInput("agegroup1", "Select Age Group:",
                              choices = unique(na.omit(nhanes$age_group)))
                ),
                mainPanel(
                  h3("Current Smoking Rate: Pre vs Post COVID"),
                  plotOutput("trendplot")
                )
              )
  ),
  
  nav_content("Ever Smoker Summary",
              sidebarLayout(
                sidebarPanel(
                  selectInput("agegroup2", "Select Age Group:",
                              choices = unique(na.omit(nhanes$age_group)))
                ),
                mainPanel(
                  h3("Ever Smoker (100+ Cigarettes)"),
                  plotOutput("everplot")
                )
              )
  ),
  
  nav_content("Under 18 Smokers",
              mainPanel(
                h3("Smokers under 18"),
                plotOutput("under18plot")
              )
  ),
  
  nav_content("Illegal Smokers",
              mainPanel(
                h3("Smokers under 21"),
                plotOutput("illegalplot")
              )
  )
)








server <- function(input, output){
  
  #  Tab 1

  output$trendplot <- renderPlot({
    df <- nhanes %>%
      filter(!is.na(current_smoker),
             age_group == input$agegroup1)
    
    df %>%
      group_by(period) %>%
      summarise(smoke_rate = mean(current_smoker, na.rm = TRUE)) %>%
      ggplot(aes(period, smoke_rate, fill = period)) +
      geom_col() +
      labs(x = "Period", y = "Smoking Rate") +
      scale_y_continuous(labels = scales::percent_format())
  })
  

  
  #  Tab 2

  output$everplot <- renderPlot({
    
    df <- nhanes %>%
      filter(!is.na(ever_smoker),
             age_group == input$agegroup2)
    
    df %>%
      count(period) %>%
      ggplot(aes(period, n, fill = period)) +
      geom_col() +
      labs(
        x = "Period",
        y = "Count of Ever Smokers",
        title = "Number of Ever Smokers (100+ Cigarettes)"
      ) +
      theme_minimal()
  })
  
  

  #  Tab 3

  output$under18plot <- renderPlot({
    
    df <- nhanes %>%
      filter(illegal_smoker == 1)
    
    df %>%
      count(period) %>%
      ggplot(aes(x = period, y = n, fill = period)) +
      geom_col() +
      labs(
        x = "Period",
        y = "Number of Smokers under 18",
        title = "Illegal Smokers by Period"
      )
  })
  
  
  

  #  Tab 4

  output$illegalplot <- renderPlot({
    df <- nhanes %>%
      filter(!is.na(illegal_smoker))
    
    df %>%
      group_by(period) %>%
      summarise(illegal_rate = mean(illegal_smoker, na.rm = TRUE)) %>%
      ggplot(aes(period, illegal_rate, fill = period)) +
      geom_col() +
      labs(x = "Period", y = "Illegal Smoking Rate") 

  })
}


shinyApp(ui = ui, server = server)



