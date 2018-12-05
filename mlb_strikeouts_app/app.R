
library(shiny)
library(tidyverse)

old_data <- read_rds("final_app_old_data")

# Define UI for application that draws a histogram

ui <- fluidPage(
   
   # Application title
  
   titlePanel("MLB Strikeout Data"),
   
   # Sidebar with a slider input for number of bins 
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("year",
                     "Year Range:",
                     min = 1918,
                     max = 2017,
                     value = c(2005, 2017)),
         
         checkboxInput("so",
                       "Display Strikeouts Per At Bat",
                       value = TRUE),
         
         checkboxInput("ba",
                       "Display Mean Batting Average",
                       value = FALSE),
         
         checkboxInput("hr",
                       "Display Homeruns Per At Bat",
                       value = FALSE)
      ),
      
      # Show a plot of the generated distribution
      
      mainPanel(
         plotOutput("scatterplot")
      )
   )
)

# Define server logic required to draw a scatterplot

server <- function(input, output) {
   
   output$scatterplot <- renderPlot({
     
     old_data %>%
       filter(input$year[1] <= year_id) %>%
       filter(input$year[2] >= year_id) %>%
       ggplot(aes(x = year_id)) +
       geom_line(aes(y = so_per_ab, color = "SO per AB"), alpha = input$so) + 
       geom_line(aes(y = mean_ba, color = "Mean BA"), alpha = input$ba) + 
       geom_line(aes(y = hr_per_ab, color = "HR per AB"), alpha = input$hr) +
       ggtitle("MLB Per At Bat Statistics Over The Years") +
       labs(x = "Year", y = "Percentage")
      
   })
}

# Run the application 

shinyApp(ui = ui, server = server)

