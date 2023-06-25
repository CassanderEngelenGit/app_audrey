
pacman::p_load(tidyverse, shiny, shinydashboard, lubridate, DT, knitr, tinytex, mapview)

#tinytex::install_tinytex()

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
      menuItem(text = "Input data", tabName = "input"),
      menuItem(text = "Mix and Match", tabName = "match"),
      menuItem(text = "Breeding pairs", tabName = "pairs"),
      downloadButton(outputId = "pdf_table", label = "report.pdf"),
      actionButton("create", "Create table"),
      actionButton("delete", "delete selected row")
    )
  ),
  
  ############################################
  
  # create the body of the different pages
  dashboardBody(
    # create different tabItems
    tabItems(
      tabItem(
        tabName = "input",
        
        fluidRow(
          box(
            # create inputs to alter of add rows to table
            textInput("field", " field number"),
            radioButtons("sex", "sex", c("male", "female")),
            radioButtons("type", "type", c("Bush", "Semi Bush", "Vine", "Netted Vine")),
            numericInput("vigor", "vigor", min = 1, max = 9, step = 1, value = 1),
            numericInput("pollen", "pollen quality", min = 1, max = 9, step = 1, value = 1),
            radioButtons("canopy", "canopy cover", choices = c("leave empty", "open", "semi-open", "closed")),
            dateInput("flowering", "flowering date"),
            textInput("background", "background"),
            textInput("notes", "notes"),
            fileInput("file1", "Choose CSV",
                      accept = ".csv"),
            actionButton("loadcsv", " load the csv"),
            actionButton("updateTable", "update row")),
          
          # plot datatable of the input csv  
          column(12, title = "datafile", 
                 DT::dataTableOutput("datafile"))
        )
      ),
      
      tabItem(
        tabName = "match",
        actionButton("start_matching", "start matching", stype = "width:800px"),
        
        fluidRow(
          h2("Select female"),
          column(12, title = "females", 
                 DT::dataTableOutput("dataFemales")),
          
          h2("Select males"),
          column(12, title = "males", 
                 DT::dataTableOutput("dataMales")),
          
          actionButton("combine", "combine selected", stype = "width:800px"),
        
          h2("Breeding pair"),
          column(12, title = "breeding pair", 
                 DT::dataTableOutput("dataPaired")))
        # end tabItem 1
        ),
      tabItem(
        tabName = "pairs",
        
        fluidRow(
          
          h2("Breeding couples"),
          
          column(12, title = "Combined table", 
                 DT::dataTableOutput("combinedTable"))
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
  
################################################################################
  # LOAD AND ALTER USER CSV FILE #
  
  # create reactive data object to load csv into
  data_csv <- reactiveVal(tibble())
  
  # allow upload of csv
  dataFile <- reactive({
    upl <- input$file1
    
    if (is.null(upl))
      return(NULL)
    
    read_csv(upl$datapath)
  })
  
  # fully load the csv into the reactive data_csv object
  observeEvent(input$loadcsv, {
    
    file <- dataFile()
    data_csv(file)
  })
  
  # update the main userInputs on the first page to the selected row
  observeEvent(input$datafile_rows_selected, {
    row <- input$datafile_rows_selected
    
    updateTextInput(session, "field", value = data_csv()[[row, "field_nr"]])
    updateRadioButtons(session, "sex", selected = data_csv()[[row, "sex"]])
    updateRadioButtons(session, "type", selected = data_csv()[[row, "type"]])
    updateNumericInput(session, "vigor", value = data_csv()[[row, "vigor"]])
    updateNumericInput(session, "pollen", value = data_csv()[[row, "pollen quality"]])
    updateRadioButtons(session, "canopy", selected = data_csv()[[row, "canopy cover"]])
    updateDateInput(session, "flowering", value = data_csv()[[row, "flowering date"]])
    updateTextInput(session, "background", value = data_csv()[[row, "background"]])
    updateTextInput(session, "notes", value = data_csv()[[row, "other notes"]])
  })
  
  # update table row based on inputs
  observeEvent(input$updateTable,{
    
    row <- input$datafile_rows_selected
    
    current_table <- data_csv()
    
    new_row <- tibble(field_nr = ifelse(is.null(input$field), NULL, input$field), 
                      sex = ifelse(is.null(input$sex), NULL, input$sex), 
                      type = ifelse(is.null(input$type), NULL, input$type),
                      vigor = ifelse(is.null(input$vigor), NULL, input$vigor),
                      pollen_quality = ifelse(is.null(input$pollen), NULL, input$pollen),
                      canopy_cover = ifelse(is.null(input$canopy), NULL, input$canopy),
                      background = ifelse(is.null(input$background), NULL, input$background),
                      notes = ifelse(is.null(input$notes), NULL, input$notes),
                      flowering_date = ifelse(is.null(input$flowering), NULL, as.character(input$flowering))) %>% 
      mutate(flowering_date = as.Date(flowering_date))
    
    current_table[row,] <- new_row
    
    
    
    data_csv(current_table)
      
  })
  
  # output the datafile
  output$datafile <- DT::renderDataTable(data_csv(), selection = "single")
  
  
################################################################################
  # SHOW AND COMBINE MALES AND FEMALES #
  
  dataMal<- reactiveVal(tibble())
  
  dataFem <- reactiveVal(tibble())
  
  observeEvent(input$start_matching, {
  
    dat_m <- dataFile() %>% 
      subset(sex == "male")
    
    dataMal(dat_m)
    
    dat_f <- dataFile() %>% 
      subset(sex == "female")
    
    dataFem(dat_f)
    
  })

  data_paired <- reactive({
    
    row_f <- input$dataFemales_rows_selected
    row_m <- input$dataMales_rows_selected
    
    tibble(field_nr = "-", 
           sex = "-", 
           type = paste("p:", dataFem()[[row_f, "type"]], "X", dataMal()[[row_m, "type"]]),
           vigor = paste("p:", dataFem()[[row_f, "vigor"]], "X", dataMal()[[row_m, "vigor"]]),
           pollen_quality = paste("p:", dataFem()[[row_f, "pollen quality"]], "X", dataMal()[[row_m, "pollen quality"]]),
           canopy_cover = paste("p:", dataFem()[[row_f, "canopy cover"]], "X", dataMal()[[row_m, "canopy cover"]]),
           background = paste("p:", dataFem()[[row_f, "field_nr"]], "X", dataMal()[[row_m, "field_nr"]]),
           other_notes = "-", 
           flowering_date = "-"
    )
    
  })
 
  
###############################################################################
  # ADD AND DELETE ROWS FOR NEW COMBINATIONS #
  
  selectedRow <- reactiveVal(NULL)
  
  combined <- reactiveVal(tibble())
  
  
  observeEvent(input$create, {
    
    row <- data_paired()[1,]
    
    existingTable <- combined()
    
    if(nrow(existingTable) == 0){
      updatedTable <- bind_rows(existingTable, row)  
      
    }
    
    else{
      if(!existingTable[nrow(existingTable),"background"] == row["background"]){
        updatedTable <- bind_rows(existingTable, row)
      }
      
      else{
        updatedTable <- existingTable
      }
    }
    
    combined(updatedTable)
    
  })
  
  
  observeEvent(input$delete, {
    
    existingTable <- combined()
    
    updatedTable <- combined()[-input$combinedTable_rows_selected,]
    
    combined(updatedTable)
  })
  
  
  
################################################################################
  
  
################################################################################
 
  output$dataFemales <-  DT::renderDataTable(dataFem(), selection = "single")
  output$dataMales <-  DT::renderDataTable(dataMal(), selection = "single")
  output$dataPaired <-  DT::renderDataTable(data_paired(), selection = "single")
  output$combinedTable <- DT::renderDataTable(combined(), selection = "single")
  
}

############################################
runApp(shinyApp(ui, server))

#################################################






