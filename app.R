
pacman::p_load(tidyverse, shiny, shinydashboard, lubridate, DT, knitr, tinytex, mapview)

#tinytex::install_tinytex()

##################
### FUNCTIONS ###
################

test.data <- tibble(program = c(1,1,1,2,2,3,3,3,3,3), type = c("A", "B", "B", "C", "D", "A", "B", "B", "C", "D"), 
                    vigour = c(4,7,8,8,5,6,7,9,10,3), 
                    shape = c("Round", "Square", "Round","Round", "Square", 
                              "Round","Round", "Square", "Round","Square"), 
                    pollen_quality = c(4,7,8,8,5,6,7,9,10,3),
                    code = c("xx2", "xy3", "xx4", "xy6", "yx7", "kw7", "ix6", "uw5", "lw7", "yd7"))

output <- tibble()

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
      menuItem(text = "Mix and Match", tabName = "match"),
      menuItem(text = "Breeding pairs", tabName = "pairs"),
      downloadButton(outputId = "pdf_table", label = "report.pdf"),
      actionButton("create", "Create table")
    )
  ),
  
  ############################################
  
  # create the body of the different pages
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "match",
        
        fluidRow(
          h2("Select plant 1"),
          column(12, title = "data1", 
                 DT::dataTableOutput("data")),
          
          h2("Select plant 2"),
          column(12, title = "data2", 
                 DT::dataTableOutput("data2")),
          
          h2("Select plant result"),
          column(12, title = "data3", 
                 DT::dataTableOutput("data3")))
        # end tabItem 1
        ),
      tabItem(
        tabName = "pairs",
        
        fluidRow(
          
          h2("Breeding couples"),
          
          column(12, title = "data4", 
                 DT::dataTableOutput("data4"))
          # end fluidRow
        )
    
        # end tabItem 2
      )
      # end tabItems
      )
    # end dashboardBody
    )
  # end ui
  )
  

############################################
server <- function(input, output, session) {
  dat <- reactive({test.data})
  
################################################################################
  
  dat2 <- reactive({
    dat() %>% 
      subset(program == dat()[[input$data_rows_selected, 1]] &
               type == dat()[[input$data_rows_selected, 2]])
  })
  
################################################################################
  
  dat3 <- reactive({
    dat()[input$data_rows_selected,] %>% 
      bind_rows(dat()[input$data2_rows_selected,]) %>% 
      bind_rows(tibble(program = dat()[[input$data_rows_selected, 1]], 
                       
                       type = dat()[[input$data_rows_selected, 2]],
                       
                       vigour = ((dat()[[input$data_rows_selected, 3]]+dat()[[input$data2_rows_selected, 3]])/2),
                       
                       shape = paste(dat()[[input$data_rows_selected, 4]], "-", dat()[[input$data2_rows_selected, 4]]),
                       
                       pollen_quality = ((dat()[[input$data_rows_selected, 5]]+dat()[[input$data2_rows_selected, 5]])/2),
                       
                       code = paste(dat()[[input$data_rows_selected, 6]], "-", dat()[[input$data2_rows_selected, 6]])
                       )
                       )
  })
  
  
  
  
###############################################################################

################################################################################
  selectedRow <- reactiveVal(NULL)
  table2 <- reactiveVal(tibble())
  
  observeEvent(input$create, {
    
    selectedRowData <- dat3()[3,]
    
    selectedRow(selectedRowData)
    
  })
  
  
  
  observeEvent(selectedRow(), {
    row <- selectedRow()
    
    existingTable <- table2()
    
    updatedTable <- bind_rows(table2(), row)
    
    table2(updatedTable)

    })
  
  
################################################################################
 
  
  
  
  
  output$data <-  DT::renderDataTable(dat(), selection = "single")
  output$data2 <-  DT::renderDataTable(dat2(), selection = "single")
  output$data3 <-  DT::renderDataTable(dat3(), selection = "single")
  output$data4 <- DT::renderDataTable(table2(), selection = "single")
  output$pdf_table <- downloadHandler(
    
    filename = "breeding_table.pdf",
    
    content = function(file){
      
      tempReport <- file.path(tempdir(),"report2.Rmd")
      
      file.copy("../report2.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(table_for_pdf = dat3())
      
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()), encoding = "UTF-8")
      
    }
  )
  
}

############################################
runApp(shinyApp(ui, server), launch.browser = T)

#################################################