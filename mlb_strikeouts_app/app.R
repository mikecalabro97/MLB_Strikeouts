library(shiny)
library(tidyverse)

old_data <- read_rds("final_app_old_data")
velo_data <- read_rds("final_app_velo_data")
pitch_type_data <- read_rds("final_app_pitch_type_data")

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  
  titlePanel("MLB Strikeout Data"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      h4("GRAPH 1"),
      
      sliderInput("year",
                  "Year Range:",
                  min = 1918,
                  max = 2017,
                  value = c(2005, 2017),
                  sep = ""),
      
      checkboxInput("so",
                    "Display Strikeouts Per At Bat",
                    value = TRUE),
      
      checkboxInput("ba",
                    "Display Mean Batting Average",
                    value = FALSE),
      
      checkboxInput("hr",
                    "Display Homeruns Per At Bat",
                    value = FALSE),
      
      h4("Interesting Combos to View:"),
      
      h5("1. All Three Stats Since 1918"),
      
      h5("2. HRs and SOs from 1955-1965"),
      
      h5("3. HRs and SOs from 2005-2017"),
      
      h6("Many people believe that the increase in strikeouts per at bat 
         over the years is due to batters swinging for homeruns more often, 
         sacrificing their contact percentage for a chance to put one out of 
         the park and earn a quick run. However, the strikeout and homerun data
         from the 50s and 60s, and from '05-'17, seem to refute this claim."),
      
      h4("GRAPH 2"),
      
      radioButtons("graph2",
                   "Which Pitch Graph Would You Like To View?",
                   c("Pitch Velocity Graph", "Pitch Type Graph"),
                   selected = "Pitch Velocity Graph"),
      
      h4("Checkboxes For Pitch Velocity Graph"),
      
      checkboxInput("fb_avg",
                    "Display Average Fastball Velocity",
                    value = TRUE),
      
      checkboxInput("fb_max",
                    "Display Max Fastball Velocity",
                    value = FALSE),
      
      checkboxInput("cu_avg",
                    "Display Average Curveball Velocity",
                    value = TRUE),
      
      checkboxInput("cu_max",
                    "Display Max Curveball Velocity",
                    value = FALSE),
      
      h4("Radio Buttons For Pitch Type Graph"),
      
      radioButtons("type",
                   "Which Type of Pitch?",
                   c("Fastball", "Curveball"),
                   selected = "Fastball",
                   inline = TRUE)
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      plotOutput("old_stats"),
      a("The data above was collected from The Lahman Baseball Database, which you can find by clicking this text!",
        href='http://www.seanlahman.com/baseball-archive/statistics/'),
      plotOutput("pitching"),
      a("The data above was collected from The Fangraphs Website, which you can find by clicking here!",
        href='https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0'),
      h5("Conclusion")
    )
  )
)

# Define server logic required to draw a scatterplot

server <- function(input, output) {
  
  output$old_stats <- renderPlot({
    
    old_data %>%
      filter(input$year[1] <= year_id) %>%
      filter(input$year[2] >= year_id) %>%
      ggplot(aes(x = year_id)) +
      geom_line(aes(y = so_per_ab, color = "SO per AB"), alpha = input$so) + 
      geom_line(aes(y = mean_ba, color = "Mean BA"), alpha = input$ba) + 
      geom_line(aes(y = hr_per_ab, color = "HR per AB"), alpha = input$hr) +
      ggtitle("MLB Per At Bat Statistics Over The Years") +
      labs(x = "Season", y = "Percentage")
    
  })
  
  output$pitching <- renderPlot({
    if (input$graph2 == "Pitch Velocity Graph") {
    velo_data %>%
      filter(! is.na(v_fa)) %>%
      filter(! is.na(v_cu)) %>%
      filter(season > 2007) %>%
      select(season, v_fa, v_cu) %>%
      group_by(season) %>%
      summarize(avg_fastball_velo = mean(v_fa),
                max_fastball_velo = max(v_fa),
                avg_curveball_velo = mean(v_cu),
                max_curveball_velo = max(v_cu)) %>%
      ggplot(aes(x = season)) +
      geom_point(aes(y = avg_fastball_velo, color = "Average Fastball Velo"), alpha = input$fb_avg) +
      geom_point(aes(y = max_fastball_velo, color = "Max Fastball Velo"), alpha = input$fb_max) +
      geom_point(aes(y = avg_curveball_velo, color = "Average Curveball Velo"), alpha = input$cu_avg) +
      geom_point(aes(y = max_curveball_velo, color = "Max Curveball Velo"), alpha = input$cu_max) +
      labs(x = "Season", y = "Velocity") +
      ggtitle("MLB Pitch Velocities Since 2008",
              subtitle = "Pitchers Are Throwing Significantly Harder Than They Used To")
    } else {
      if (input$type == "Fastball") {
      pitch_type_data %>%
        ggplot(aes(x = season, y = avg_fastball_percentage)) + 
        geom_point(aes(color = "Fastball Percentage")) +
        geom_smooth(method = "lm") +
        labs(x = "Season", y = "Average Fastball Percentage") +
        ggtitle("MLB Pitch Types Since 2008", subtitle = "Pitchers Are Throwing Less Fastballs Than They Used To")
      } else {
        pitch_type_data %>%
          ggplot(aes(x = season, y = avg_curveball_percentage)) + 
          geom_point(aes(color = "Curveball Percentage")) +
          geom_smooth(method = "lm") +
          labs(x = "Season", y = "Average Curveball Percentage") +
          ggtitle("MLB Pitch Types Since 2008", "Pitchers Are Throwing More Curveballs Than They Used To")
      }
    }
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
