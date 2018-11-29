rm(list=ls())
# user-defined functions ---------------------------------------------------------------
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

# UI script ----------------------------------------------------------------------

# * Main page layout --------------------------------------------------------
# For this example, we just use a simple fluidpage layout. This just means a page that allows you to place panels inside of it.
# Here, we use a side panel on the left, containing all of the settings and some output to show the interactivity of the app.
# These include: a button to load your own csv data file in, a multiplier (just multiplies the sum of the columns in table), 
#   and an output table. This output table contains the column sums of the edited table, which updates in reaction to you changing anything
ui <- fluidPage(
  # ** side bar for settings on left ----------------------------------------
  #NOTE: Left is the default position, if you want it somewhere else you can set it to right as well. See:
  # https://shiny.rstudio.com/reference/shiny/latest/sidebarLayout.html
  shiny::sidebarPanel(
    #fileinput allows you to load in a file. See https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
    #This creates input$usr_file, using the inputId argument
    fileInput(
      inputId = "usr_file",
      label = "load file",
      accept = "text/CSV" #This is called a MIME type. basically it won't let you load in a file that isn't a csv or text
      ),
    #Simply multiplies the final table which is to be used in the calculations by the number the user provides (default is 1)
    numericInput(
      inputId = "raw_mult",
      label = "multiply the data by:",
      value = 1
    ),
    #help text is useful for providing some instruction to the user
    helpText("Sum of columns"),
    #Just a table of the results to show off the interactivity
    tableOutput("final_colsums")
  ),

  # ** output panel of results ----------------------------------------------
  #main panel: Show me a table of some placeholder or data I've loaded in, and another table after that, which is the data.frame to be used in the calculations
  # Then show me a table with column sums
  shiny::mainPanel(
    #data as a hands-on table, which you can edit (see https://jrowen.github.io/rhandsontable/)
    rHandsontableOutput("raw_data_table"),
    #the resulting table that is getting passed back to r (see https://www.rdocumentation.org/packages/rhandsontable/versions/0.3.6/topics/hot_to_r)
    tableOutput("user_edited_table")
  )
)


# Server script ------------------------------------------------------------------

#The "server" in shiny is actually a function which has inputs, outputs and can even handle multiple sessions going on at once (advanced)
#The inputs defined in the UI like "usr_file" above are all put into a list object called input. to access these items just use the $ sign
# as you would normally in R. For example input$usr_file is a table with 3 elements, name, size and datapath. 
# you can see below that I use input$usr_file$datapath to open the csv file that the user has selected
server <- function(input, output, session) {
  #The below creates a "reactive" object. This is referred to thereafter as name() where name is the name of the object (i.e. user_data())
  #essentially R sees it as a function within the server function, hence the brackets.
  user_data <- reactive({
    
    if(is.null(input$usr_file)){
      #if the user hasn't loaded a file yet (i.e. input$usr_file, "usr_file" in ui is "null" meaning it doesn't exist), then do nothing
      return()
    } else {
      #if the user has loaded a file, then read the csv. shiny loads the file into a temp directory for security reasons. the file is deleted when the app closes.
      read.csv(input$usr_file$datapath)
    }
      
  })
  
    
  # * Render excel-like table for user ----------------------------------------
  #There is a javascript object called a handsontable. This works kind of like excel (but not as good)
  #It is sufficient for basic editing, but not much more than that.
  #The aim here is to take the user's file (or some example data if they haven't loaded a file yet)
  # provide it as a handsontable to the user, allow the user to edit it, take the EDITED data, pass that back to R
  # manipulate the data in the R server, present the "final" results in the shiny app as another table, with some
  # summary statistics printing out to a 3rd table, located in the sidepanel1
  
  #make the hands on table out of example data, or whatever csv the user loads in
  output$raw_data_table <- renderRHandsontable({
    
    if(is.null(input$usr_file)) {
      #if the user hasn't loaded in a file, then make an example table
      placeholder_table <- rhandsontable(
        data.frame(
          placeholder1 = c(1, 2, 3),
          placeholder2 = c(1, 2, 3),
          placeholder3 = c(1, 2, 3)
        ),
        height = 400,
        selectCallback = TRUE,
        readOnly = FALSE
      ) %>% 
        hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = TRUE
        )
      #try to make it as editable as possible using the hot_ commands from the rHandsonTable package
      hot_cols(
        hot = placeholder_table,
        columnSorting = TRUE,
        manualColumnResize = TRUE,
        manualColumnMove = TRUE
      )

    } else {
      #If the user has loaded some data, make a handsontable out of it (i.e. make it out of user_data())
      usr_table <- rhandsontable(
        user_data(),
        height = 400,
        selectCallback = TRUE,
        readOnly = FALSE
      ) %>% 
        hot_context_menu(
          allowRowEdit = TRUE,
          allowColEdit = TRUE
        )
      #try to make it as editable as possible using the hot_ commands from the rHandsonTable package
      hot_cols(
        hot = usr_table,
        columnSorting = TRUE,
        manualColumnResize = TRUE,
        manualColumnMove = TRUE
      )
    }
  })
  


  # Output tables -----------------------------------------------------------
  #Keep a reactive data.frame up to date all the time! this should be the data.frame used in calculations
  DATA_USED <- reactive({hot_to_r(input$raw_data_table) * input$raw_mult})
  #test performing numerical computation on the table - tests whether you can use the live values in the table, and shows off how it behaves!
  output$user_edited_table <- renderTable({DATA_USED()})
  #provide some visual feedback, just the sum of the columns for reference
  output$final_colsums <- renderTable({t(colSums(DATA_USED()))})

}

shinyApp(ui = ui,server = server)
