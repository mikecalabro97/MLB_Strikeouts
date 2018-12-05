
#For comments describing all the data shown in the app,
#As well as more data and visualizations on the subject,
#VIEW MY TEST.RMD FILE

#It includes my data finding process, and thought procss regarding
#which statistics I would investigate


library(shiny)
library(tidyverse)

old_data <- read_rds("final_app_old_data")
velo_data <- read_rds("final_app_velo_data")
pitch_type_data <- read_rds("final_app_pitch_type_data")

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  
  titlePanel("MLB Strikeout Data"),
  
  # Sidebar contains a lot
  
  sidebarLayout(
    sidebarPanel(
      
      #Everything under Graph 1 and above Graph 2 are meant to effect Graph 1 only
      
      h4("GRAPH 1"),
      
      #Includes a slider input to decide the range of seasons you are viewing
      
      sliderInput("year",
                  "Year Range:",
                  min = 1918,
                  max = 2017,
                  value = c(2005, 2017),
                  sep = ""),
      
      #Includes checkbox inputs, from which you can select 1, 2 or all
      #SO displays strikeouts per at bat
      #BA displays batting averages, or number of hits per number of at bats
      #HR displays homeruns per at bat
      
      checkboxInput("so",
                    "Display Strikeouts Per At Bat",
                    value = TRUE),
      
      checkboxInput("ba",
                    "Display Mean Batting Average",
                    value = FALSE),
      
      checkboxInput("hr",
                    "Display Homeruns Per At Bat",
                    value = FALSE),
      
      #Below are some interesting ways to view the data - things I found insightful
      
      h4("Interesting Combos to View:"),
      
      h5("1. All Three Stats Since 1918"),
      
      h5("2. HRs and SOs from 1955-1965"),
      
      h5("3. HRs and SOs from 2005-2017"),
      
      #Below is some commentary on the data
      
      h6("Many people believe that the increase in strikeouts per at bat 
         over the years is due to batters swinging for homeruns more often, 
         sacrificing their contact percentage for a chance to put one out of 
         the park and earn a quick run. However, the strikeout and homerun data
         from the 50s and 60s, and from '05-'17, seem to refute this claim."),
      
      #Everything below this in the slider is for graph 2
      
      h4("GRAPH 2"),
      
      #These decide which graph you ar going to view - velo's or pitch types
      
      radioButtons("graph2",
                   "Which Pitch Graph Would You Like To View?",
                   c("Pitch Velocity Graph", "Pitch Type Graph"),
                   selected = "Pitch Velocity Graph"),
      
      #These checkboxes decide which statistics you view 
      
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
      
      #These radio buttons decide if you will look at a graph of fastballs or curveballs
      
      h4("Radio Buttons For Pitch Type Graph"),
      
      radioButtons("type",
                   "Which Type of Pitch?",
                   c("Fastball", "Curveball"),
                   selected = "Fastball",
                   inline = TRUE)
    ),
    
    #On the main panel, I display two graphs, one for homeruns, strikeouts, and batting average
    #And one for pitching since 2007
    #The commentary for the strikeouts and homeruns appears in the side panel
    #The commentary for the pitching is at the bottom of the page
    
    mainPanel(
      
      #Old stats is what displays graph 1
      
      plotOutput("old_stats"),
      
      #citing my sources
      
      a("The data above was collected from The Lahman Baseball Database, which you can find by clicking this text!",
        href='http://www.seanlahman.com/baseball-archive/statistics/'),
      
      #A way to separate the two graphs
      
      h4("________________________________________________________________"),
      
      #Pitching displays the pitching graphs (naturally)
      
      plotOutput("pitching"),
      
      #Links to the source (Same as above)
      
      a("The data above was collected from The Fangraphs Website, which you can find by clicking here!",
        href='https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2018&month=0&season1=2018&ind=0&team=0&rost=0&age=0&filter=&players=0'),
      
      #Here is the commentary
      
      h6("From these pitching graphs, we see a few very clear conclusions."),
      h6("1. Pitchers Are throwing harder. The average fastball velocity jumped from 91 MPH in '07 to 93 MPH in '18. 
         Similarly, The average fastball velocity jumped from just over 75 MPH in '07 to 78 MPH in '18.
         This may not seem like too significant of an increase, but from a hitter's perspective this is a big change."),
      h6("2. Pitchers are throwing less fastballs and more curveballs. We see pretty clearly from the graphs that 
         pitchers are starting to become more comfortable throwing curveballs to batters. A typical reason why a pitcher would
         use a fastball so often is because it is more accurate, and easier to place within a small window. Since curveballs are rising,
         it seems plausible that pitchers are becoming more confident in the accuracy of their curveballs, a pitch which is harder to hit."),
      h6("With these facts in mind, the data seems to support a hypothesis that the main reason why players are striking out more often
         is because pitchers are improving their skills at a rate which batters cannot keep up with."),
      h6("Shiny App Created by Michael Calabro")
      
    )
  )
)

# Define server logic required to draw my scatterplots

server <- function(input, output) {
  
  output$old_stats <- renderPlot({
    
    #Uses the old_batting_filtered data from the playground and the test rmd
    
    #BIGGEST NOTE - Changing the alpha's to either 0 or 1 is the way that I was
    #able to either display the lines/points or not display them
    #You can see this at the end of most of the geom_point and geom_lines
    
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
  
  #Velo_data is using the pitch_velocities data from the playground and rmd
  #The large if-elses allow me to display different stats, since I could not
  #combine them into one table
  
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
      
      #pitch_type_data is using the fastball_percent_summary data from the playground and rmd
      
      if (input$type == "Fastball") {
      pitch_type_data %>%
        ggplot(aes(x = season, y = avg_fastball_percentage)) + 
        geom_point(aes(color = "Fastball Percentage")) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Season", y = "Average Fastball Percentage") +
        ggtitle("MLB Pitch Types Since 2008", subtitle = "Pitchers Are Throwing Less Fastballs Than They Used To")
      } else {
        pitch_type_data %>%
          ggplot(aes(x = season, y = avg_curveball_percentage)) + 
          geom_point(aes(color = "Curveball Percentage")) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(x = "Season", y = "Average Curveball Percentage") +
          ggtitle("MLB Pitch Types Since 2008", "Pitchers Are Throwing More Curveballs Than They Used To")
      }
    }
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
