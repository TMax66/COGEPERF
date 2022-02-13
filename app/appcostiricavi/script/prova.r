library(rpivotTable)
# consider using tidyverse library call to get dplyr, readr and rvest
# library(tidyverse)
library(dplyr)
library(readr)
# need rvest to be able to 'scrape' rPivotTable
library(rvest)
library(shiny)
# library(openxlsx)
# I really like how lightweight and versatile writexl is
library(writexl)
# need JS functionality in htmlwidgets
library(htmlwidgets)
library(shinyjs)
library(clipr)

#ui
ui = fluidPage(
  # for the purposes of this exercise, I'm only including csv and xlsx to simplify the download logic
  # but you could certainly add more format options
  radioButtons(inputId = "format", label = "Enter the format to download", 
               choices = c( "csv", "excel"), inline = FALSE, selected = "csv"),
  downloadButton("download_pivot"),
  actionButton("copy_pivot", "Copy"),
  fluidRow(rpivotTableOutput("pivot")))

#server
server = function (input, output) { 
  
  output$pivot <- renderRpivotTable(
    rpivotTable(dtanalisi,
                onRefresh = htmlwidgets::JS(
                  "function(config) {
                            Shiny.onInputChange('pivot', document.getElementById('pivot').innerHTML); 
                        }"))
  )
  
  # create an eventReactive dataframe that regenerates anytime the pivot object changes
  # wrapped in a tryCatch to only return table object. errors out when charts are shown
  pivot_tbl <- eventReactive(input$pivot, {
    tryCatch({
      input$pivot %>%
        read_html %>%
        html_table(fill = TRUE) %>%
        .[[2]]
    }, error = function(e) {
      return()
    })
  })
  
  # allow the user to download once the pivot_tbl object is available
  observe({
    if (is.data.frame(pivot_tbl()) && nrow(pivot_tbl()) > 0) {
      shinyjs::enable("download_pivot")
      shinyjs::enable("copy_pivot")
    } else {
      shinyjs::disable("download_pivot")
      shinyjs::disable("copy_pivot")
    }
  })
  
  # using shiny's download handler to get the data output
  output$download_pivot <- downloadHandler(
    filename = function() {
      if (input$format == "csv") {
        "pivot.csv"
      } else if (input$format == "excel") {
        "pivot.xlsx"
      }
    },
    content = function(file) {
      if (input$format == "csv") {
        write_csv(pivot_tbl(), path = file)
      } else if (input$format == "excel") {
        writexl::write_xlsx(pivot_tbl(), path = file)
      }
    }
  )
  
  # copy pivot table - works natively on Windows/OSX. Requires xclip on Linux
  observeEvent(input$copy_pivot,  {
    clipr::write_clip(pivot_tbl(), object_type = "table")
  })
  
}

shinyApp(ui = ui, server = server)