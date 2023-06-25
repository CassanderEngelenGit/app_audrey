
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
      menuItem(text = "Breeding pairs", tabName = "pairs")
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
            dateInput("flowering", "flowering date", value = as.Date(NA)),
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
        actionButton("start_matching", "start matching", style = "width:800px"),
        
        fluidRow(
          h2("Select female"),
          column(12, title = "females", 
                 DT::dataTableOutput("dataFemales")),
          
          h2("Select males"),
          column(12, title = "males", 
                 DT::dataTableOutput("dataMales")),
          
          actionButton("combine", "combine selected"),
        
          h2("Breeding pair"),
          column(12, title = "breeding pair", 
                 DT::dataTableOutput("dataPaired"))),
          actionButton("create", "add to breeding table")
        # end tabItem 1
        ),
      tabItem(
        tabName = "pairs",
        
        fluidRow(
          box(
          # create inputs to alter of add rows to table
            textInput("field2", " field number"),
            radioButtons("sex2", "sex", c("male", "female"), selected = character(0)),
            radioButtons("type2", "type", c("Bush", "Semi Bush", "Vine", "Netted Vine"), selected = character(0)),
            numericInput("vigor2", "vigor", min = 1, max = 9, step = 1, value = NA),
            numericInput("pollen2", "pollen quality", min = 1, max = 9, step = 1, value = NA),
            radioButtons("canopy2", "canopy cover", choices = c("leave empty", "open", "semi-open", "closed"), selected = character(0)),
            dateInput("flowering2", "flowering date", value = as.Date(NA)),
            textInput("background2", "background"),
            textInput("notes2", "notes"),
            actionButton("updateTable2", "update row"),
            actionButton("delete", "delete selected row")
          ),
        
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
    
    new_row <- tibble(field_nr = ifelse(is.null(input$field), NA, input$field), 
                      sex = ifelse(is.null(input$sex), NA, input$sex), 
                      type = ifelse(is.null(input$type), NA, input$type),
                      vigor = ifelse(is.null(input$vigor), NA, input$vigor),
                      pollen_quality = ifelse(is.null(input$pollen), NA, input$pollen),
                      canopy_cover = ifelse(is.null(input$canopy), NA, input$canopy),
                      background = ifelse(is.null(input$background), NA, input$background),
                      notes = ifelse(is.null(input$notes), NA, input$notes),
                      flowering_date = ifelse(is.null(input$flowering), NA, as.character(input$flowering))) %>% 
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
    
    tibble(field_nr = NA, 
           sex = NA, 
           type = NA,
           vigor = NA,
           pollen_quality = NA,
           canopy_cover = NA,
           background = paste("p:", dataFem()[[row_f, "field_nr"]], "X", dataMal()[[row_m, "field_nr"]]),
           other_notes = NA, 
           flowering_date = NA
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
  # ALTER TABLE OF BREEDING PAIRS #
  # update the main userInputs on the first page to the selected row
  observeEvent(input$combinedTable_rows_selected, {
    row <- input$combinedTable_rows_selected
    
    updateTextInput(session, "field2", value = combined()[[row, "field_nr"]])
    updateRadioButtons(session, "sex2", selected = combined()[[row, "sex"]])
    updateRadioButtons(session, "type2", selected = combined()[[row, "type"]])
    updateNumericInput(session, "vigor2", value = combined()[[row, "vigor"]])
    updateNumericInput(session, "pollen2", value = combined()[[row, "pollen quality"]])
    updateRadioButtons(session, "canopy2", selected = combined()[[row, "canopy cover"]])
    updateDateInput(session, "flowering2", value = combined()[[row, "flowering date"]])
    updateTextInput(session, "background2", value = combined()[[row, "background"]])
    updateTextInput(session, "notes2", value = combined()[[row, "other notes"]])
  })
  
  # update table row based on inputs
  observeEvent(input$updateTable2,{
    
    row <- input$combinedTable_rows_selected
    
    current_table <- combined()
    
    new_row <- tibble(field_nr = ifelse(is.null(input$field2), NA, input$field2), 
                      sex = ifelse(is.null(input$sex2), NA, input$sex2), 
                      type = ifelse(is.null(input$type2), NA, input$type2),
                      vigor = ifelse(is.null(input$vigor2), NA, input$vigor2),
                      pollen_quality = ifelse(is.null(input$pollen2), NA, input$pollen2),
                      canopy_cover = ifelse(is.null(input$canopy2), NA, input$canopy2),
                      background = current_table[[row, "background"]],
                      notes = ifelse(is.null(input$notes2), NA, input$notes2),
                      flowering_date = ifelse(is.null(input$flowering2), NA, as.character(input$flowering2))) %>% 
      mutate(flowering_date = as.Date(flowering_date))
    
    current_table[row,] <- new_row
    
    
    
    combined(current_table)
    
  })
  
  
################################################################################
 
  output$dataFemales <-  DT::renderDataTable(dataFem(), selection = "single")
  output$dataMales <-  DT::renderDataTable(dataMal(), selection = "single")
  output$dataPaired <-  DT::renderDataTable(data_paired(), selection = "single")
  output$combinedTable <- DT::renderDataTable(combined(), selection = "single")
  
}

############################################
runApp(shinyApp(ui, server))

#################################################






