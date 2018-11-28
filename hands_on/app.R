rm(list=ls())
# functions ---------------------------------------------------------------
#function to automatically load and/or install packages
dbloader <- function(packs) {
  system.time(
    if (length(setdiff(packs, rownames(installed.packages()))) > 0) {
      print("you need the following packages to run this script")
      print(setdiff(packs, rownames(installed.packages())))
      print("Installing them now...")
      install.packages(setdiff(packs, rownames(installed.packages())))
      print("Now loading libraries...")
      sapply(packs, require, character.only = TRUE)
    } else {
      print("All packages are installed already")
      print("Loading the specified libraries...")
      sapply(packs, require, character.only = TRUE)
    }
  )
}





# preamble ----------------------------------------------------------------
#packages used
dbloader(c("shiny","tidyverse","rhandsontable"))


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  #simple well panel for the example
  shiny::sidebarPanel(
    #fileinput allows you to load in a file
    fileInput(
      inputId = "usr_file",
      label = "load file",
      accept = "text/CSV" #This is called a MIME type. basically it won't let you load in a file that isn't a csv or text
      ),
    #Simply multiplies the final table which is to be used in the calculations by the number the user provides (default is x1)
    numericInput(
      inputId = "raw_mult",
      label = "multiply the data by:",
      value = 1
    ),
    helpText("Sum of columns"),
    tableOutput("final_colsums")
  ),
  #main panel: Show me a table of some placeholder or data I've loaded in, and another table after that, which is the data.frame to be used in the calculations
  # Then show me a table with column sums
  shiny::mainPanel(
    rHandsontableOutput("Table1"),
    tableOutput("final_table")
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  #reactive data based on the user loading a file!
  user_data <- reactive({
    
    if(is.null(input$usr_file)){
      #if the user hasn't loaded a file yet (i.e. input$usr_file, "usr_file" in ui is "null" meaning it doesn't exist), then do nothing
      return()
    } else {
      #if the user has loaded a file, then read the csv. shiny loads the file into a temp directory for security reasons. the file is deleted when the app closes.
      read.csv(input$usr_file$datapath)
    }
      
  })
  
    
  #Render 1st table at the begining 
  output$Table1 <- renderRHandsontable({
    if(is.null(input$usr_file)) {
      rhandsontable(data.frame(placeholder1 = c(1,2,3),placeholder2 = c(1,2,3),placeholder3 = c(1,2,3)), height = 400, selectCallback = TRUE, readOnly = FALSE)  %>%
        hot_cols(columnSorting = TRUE)
    } else {
      rhandsontable(user_data(), height = 400, selectCallback = TRUE, readOnly = FALSE)  %>%
        hot_cols(columnSorting = TRUE)
    }
  })
  
  observe({
    if (!is.null(input$Table1)) {
      # Render the 2nd table only after the data in the 1st table changes
      
      output$Table2 <- renderRHandsontable({
        rhandsontable(hot_to_r(input$Table1),
                      height = 400,
                      readOnly = TRUE)
      })
    }
  })
  
  #Keep a reactive data.frame up to date all the time! this should be the data.frame used in calculations
  DATA_USED <- reactive({hot_to_r(input$Table1) * input$raw_mult})
  
  #test performing numerical computation on the table - tests whether you can use the live values in the table, and shows off how it behaves!
  output$final_table <- renderTable({DATA_USED()})
  #provide some visual feedback, just the sum of the columns for reference
  output$final_colsums <- renderTable({t(colSums(DATA_USED()))})
  
}

shinyApp(ui = ui,server = server)
