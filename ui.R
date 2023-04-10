#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, shinyWidgets, readr, sf, vctrs, tmap, spatstat, sfdep, tidyr, tidyverse, maptools)

# Define UI for application that draws a histogram
fluidPage(
  
    navbarPage("Dr Go Where",
               fluid = TRUE,
               collapsible = TRUE,
               tabPanel("Home",
                        fluidPage(
                           h1("Build in Progress")
                          )
                        ),
               tabPanel("Data",
                        fluidPage(
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     br()
                            ),
                            tabPanel("Data Visualisation",
                               br(),
                               sidebarLayout(
                                 sidebarPanel(
                                   prettyRadioButtons(inputId = "aspatialDataQn",
                                    label = "Medical Facility:",
                                    choices = c("General Practitioners (GPs)",
                                                "Hospitals",
                                                "Polyclinics",
                                                "Nursing Homes",
                                                "CHAS Clinics",
                                                "Primary Care Networks (PCN)"
                                                ),
                                    selected = "General Practitioners (GPs)")
                                  ),
                                 mainPanel(
                                   tmapOutput("aspatialDataPlot",
                                              width = "100%",
                                              height = 400)
                                  )
                                 )
                                )
                               ))
                ),
     tabPanel("Kernel Density Estimation",
        fluidPage(
         sidebarLayout(
           sidebarPanel(
             prettyRadioButtons(inputId = "KDEQn",
                label = "Medical Facility:",
                choices = c("General Practitioners (GPs)",
                            "Hospitals",
                            "Polyclinics",
                            "Nursing Homes",
                            "CHAS Clinics",
                            "Primary Care Networks (PCN)"
                ),
                selected = "General Practitioners (GPs)"
              ),
             prettyRadioButtons(inputId = "KDEBandwidthQn",
                                label = "Automatic Bandwidth Selection Method:",
                                choices = c("Cross Validated (bw.diggle)" = "bw.diggle",
                                            "Cronie-Van Lieshout's Criterion (bw.CvL)" = "bw.CvL",
                                            "Scott's Rule (bw.scott)" = "bw.scott",
                                            "Likelihood Cross Validation (bw.ppl)" = "bw.ppl"
                                ),
                                selected = "bw.diggle"
             ),
             prettyRadioButtons(inputId = "KDEKernelQn",
                                label = "Kernel:",
                                choices = c("Gaussian" = "gaussian",
                                            "Epanechnikov" = "epanechnikov",
                                            "Quartic" = "quartic",
                                            "Disc" = "disc"
                                ),
                                selected = "gaussian"
             )
        ),
         mainPanel(
           plotOutput("KDEDataPlot",
              width = "100%",
              height = 400)
           )
        ))
     ),
     tabPanel("Accessbility")
  )
)