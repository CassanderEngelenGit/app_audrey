### LOAD / INSTALL PACKAGES
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(tidyverse, shiny, shinydashboard, lubridate)

##################
### FUNCTIONS ###
################

test.data <- tibble(program = c(1,1,1,2,2,3,3,3,3,3), type = c("A", "B", "B", "C", "D", "A", "B", "B", "C", "D"), 
                    vigour = c(4,7,8,8,5,6,7,9,10,3), 
                    shape = c("Round", "Square", "Round","Round", "Square", 
                              "Round","Round", "Square", "Round","Square"), 
                    pollen_quality = c(4,7,8,8,5,6,7,9,10,3))


######################
### LOAD DATASETS ###
####################

ui <- dashboardPage(
  
  # create header
  dashboardHeader(title = "Plant pollination", titleWidth = 300),
  
  ############################################
  
  # create sidebar menu items
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Mix and Match", tabName = "match")
    )
  ),
  
  ############################################
  
  # create the body of the different pages
  dashboardBody()
  
  
)
############################################
server <- function(input, output, session) {
  dat <- reactive{
    test.data %>% 
      subset(program %in% input$program &
               type %in% input$type &
               vigour %in% input$vigour &
               shape %in% input$shape &
               pollen_quality %in% input$pollen_quality)
  }
  
}

############################################
runApp(shinyApp(ui, server), launch.browser = T)

#################################################