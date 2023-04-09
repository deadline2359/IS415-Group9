#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



pop_data <- read_csv("data/aspatial/Resident Population 2015.csv", skip=11)[1:379,1:19]

# Define server logic required to draw a histogram
function(input, output, session) {
    aspatialDataInput <- reactive({
      switch(input$aspatialDataQn, 
             "Resident Population" = aspatialData$dataChosen,
             )
    })

    output$aspatial_data_table  <- renderDataTable(head(pop_data, 10))

}
