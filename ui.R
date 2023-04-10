#
pacman::p_load(shiny, shinyWidgets, vctrs, tmap)

# Define UI for application that draws a histogram
fluidPage(
  
    navbarPage("Dr Go Where",
               fluid = TRUE,
               collapsible = TRUE,
               tabPanel("Home",
                        fluidPage(
                           h1("Analysing the Accessibility of Medical Facilities")
                          )
                        ),
               tabPanel("Data Visualisation",
                        fluidPage(
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput(inputId = "aspatialDataQn",
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
                                              height = 400),
                                   p("Note: Graphs may take a while to load. Thank you for your patience.")
                                  )
                                 )
                                )
                ),
     tabPanel("Kernel Density Estimation",
        fluidPage(
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId = "KDEQn",
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
              height = 400),
           p("Note: Graphs may take a while to load. Thank you for your patience.")
           )
        ))
     ),
     tabPanel("Accessibility",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "accDataQn",
                                       label = "Medical Facility:",
                                       choices = c("Hospitals",
                                                   "Polyclinics",
                                                   "Nursing Homes",
                                                   "CHAS Clinics",
                                                   "Primary Care Networks (PCN)",
                                                   "Eldercare"
                                       ),
                                       selected = "Hospitals"
                    ),
                    prettyRadioButtons(inputId = "accFunQn",
                                       label = "Accessibility Measure Function:",
                                       choices = c("Hansen",
                                                   "KD2SFCA",
                                                   "SAM",
                                                   "2SFCA"
                                       ),
                                       selected = "Hansen"
                    ),
                    prettyRadioButtons(inputId = "accColourQn",
                                       label = "Colour:",
                                       choices = c("Red" = "Reds",
                                                   "Blue" = "Blues",
                                                   "Green" = "Greens",
                                                   "Orange" = "Oranges",
                                                   "Purple" = "Purples"
                                       ),
                                       selected = "Oranges"
                    )
                  ),
                  mainPanel(
                    tmapOutput("accessibilityPlot",
                               width = "100%",
                               height = 400),
                    p("Note: Graphs may take a while to load. Thank you for your patience.")
                  )
                  )
                )
              )
     )
  )