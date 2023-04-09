#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, shinyWidgets, readr, sf, tmap, spatstat, sfdep, tidyverse, maptools, raster, SpatialAcc, ggstatsplot, reshape2, rgdal, spNetwork)

# Define UI for application that draws a histogram
fluidPage(
  
    navbarPage("Dr Go Where",
               fluid = TRUE,
               collapsible = TRUE,
               tabPanel("Home",
                        fluidPage(
                          tabsetPanel(
                            sidebarLayout(
                              sidebarPanel(),
                              mainPanel(
                                p("Build in Progress")
                              )
                            )
                          )
                        )
                        ),
               tabPanel("Data",
                        fluidPage(
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     br()
                            ),
                            tabPanel("Aspatial Data",
                                     br(),
                                     sidebarLayout(
                                       sidebarPanel(
                                         prettyRadioButtons(inputId = "aspatialDataQn", 
                                                            label = "Show Data:", 
                                                            choices = c("Resident Population",
                                                                        "General Practitioner", 
                                                                        "Hospitals",
                                                                        "Polyclinics",
                                                                        "Nursing Homes",
                                                                        "Primary Care Networks"),
                                                            selected = "Resident Population")
                                       ),
                                       mainPanel(tableOutput('aspatial_data_table'))
                                     )
                            ),
                            tabPanel("Geospatial Data",
                                     br()
                            )
                ))),
               tabPanel("EDA"),
               tabPanel("Kernel Density Estimation"),
               tabPanel("Accessbility")),
)
