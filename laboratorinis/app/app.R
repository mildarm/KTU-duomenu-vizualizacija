library(shiny)
library(tidyverse)
library(lubridate)



ui <- fluidPage(
  
  titlePanel("Reklamos agenturu veikla"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Imones kodas", choices = NULL, selected = NULL)
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentele", tableOutput("table"))
    )
    
    )
  )
)
server <- function(input, output, session) {
  
  data <- read_csv("https://github.com/mildarm/KTU-duomenu-vizualizacija/blob/main/laboratorinis/data/lab_sodra.csv")
  
  data1<-data %>% 
    filter(ecoActCode == 731100) %>%
    arrange(code)
  
  updateSelectizeInput(session, "imones_kodas", choices = data1$code, server = TRUE)
  
  output$table <- renderTable(
    data1 %>%
      filter(code == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data1 %>%
      filter(code == input$imones_kodas) %>%
      mutate(month = ym(month)) %>%
      ggplot(aes(x = month, y = avgWage, color=name)) +
      geom_line()   
    
  )
}
shinyApp(ui, server)
