#
pacman::p_load(shiny, shinyWidgets, tmap, tidyverse, shinythemes)

# Define UI for application that draws a histogram
fluidPage(
  theme = shinytheme("flatly"),
    navbarPage("Dr Go Where",
               id = "tabs",
               fluid = TRUE,
               collapsible = TRUE,
               
               tabPanel("Home",
                        fluidPage(
                           h1("Analysing the Accessibility of Medical Facilities"),
                           a("GitHub Repo", href="https://github.com/deadline2359/IS415-Group9"),
                           h2("Problem Statement"),
                           p("In Singapore, our country is experiencing a rapidly ageing population, around one in four citizens, will be aged 65 and above by 2030. In response to this issue, the Ministry of Health (MOH) and the government have been making plans to increase the number of healthcare facilities in Singapore. 
                              In the Masterplan 2020, MOH highlighted its priorities on the shift towards community care and to provide more accessible care for elderly patients, by building community care facilities such as nursing homes, home care services, and day rehabilitation centers. 
                             The aim of these policies is to improve the continuum of care for elderly patients, reduce readmission rates and ease the burden on acute hospitals. Hence, there was also a growing need for effective distribution of health services and for healthcare to be more accessible within the community."),
                           h2("Objectives of our Project"),
                           p("Our project aims to utilize R methods to conduct spatial point pattern analysis of healthcare distribution and geographical modelling of maps. 
                             The main objective is to determine the accessibility and distribution of healthcare services for both the elderly population and Singaporeans of all ages.")
                          )
                        ),
               tabPanel("Data Visualisation",
                        value = "aspatialDataTab",
                        fluidPage(
                               sidebarLayout(
                                 sidebarPanel(
                                   selectizeInput(inputId = "aspatialDataQn",
                                                  label = "Medical Facility:",
                                                  choices = c("General Practitioners (GPs)",
                                                              "Hospitals",
                                                              "Polyclinics",
                                                              "Nursing Homes",
                                                              "CHAS Clinics",
                                                              "Primary Care Networks (PCN)"
                                                              ),
                                    selected = "General Practitioners (GPs)"),
                                   prettyRadioButtons(inputId = "aspatialBGColourQn",
                                                      label = "Colour of Background:",
                                                      choices = c("Red" = "Reds",
                                                                  "Blue" = "Blues",
                                                                  "Green" = "Greens",
                                                                  "Orange" = "Oranges",
                                                                  "Purple" = "Purples"
                                                      ),
                                                      selected = "Oranges"
                                    ),
                                   prettyRadioButtons(inputId = "aspatialSymbolColourQn",
                                                      label = "Colour of Symbol:",
                                                      choices = c("Red" = "red",
                                                                  "Blue" = "blue",
                                                                  "Green" = "green",
                                                                  "Orange" = "orange",
                                                                  "Purple" = "purple"
                                                      ),
                                                      selected = "blue"
                                   ),
                                   fluidRow(
                                     column(
                                       12,
                                       actionButton("aspatialButton", "Generate"), 
                                       align = "center"
                                     )
                                   )
                                  ),
                                 mainPanel(
                                   conditionalPanel(condition = "input.tabs == 'aspatialDataTab' && input.aspatialButton > 0", tmapOutput("aspatialDataPlot",
                                                                                       width = "100%",
                                                                                       height = "80vh")),
                                   conditionalPanel(condition = "input.tabs == 'aspatialDataTab' && input.aspatialButton == 0", column(width = 12,
                                                                                            align="center",
                                                                                            style = "height: 80vh; padding: 20px",
                                                                                            h1("Please choose your choices from the panel and click the button.")
                                                                                           )),
                                   p("Note: Graphs may take a while to load. Thank you for your patience.")
                                  )
                                 )
                                )
                ),
     tabPanel("Kernel Density Estimation",
              value = "KDETab",
        fluidPage(
         sidebarLayout(
           sidebarPanel(
             selectizeInput(inputId = "KDEQn",
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
             ),
             fluidRow(
               column(
                 12,
                 actionButton("KDEButton", "Generate"), 
                 align = "center"
               )
             )
        ),
         mainPanel(
           conditionalPanel(condition = "input.tabs == 'KDETab' && input.KDEButton > 0", plotOutput("KDEDataPlot",
                                                                       width = "100%",
                                                                       height = "80vh")),
           conditionalPanel(condition = "input.tabs == 'KDETab' && input.KDEButton == 0", column(width = 12,
                                                                    align="center",
                                                                    style = "height: 80vh; padding: 20px",
                                                                    h1("Please choose your choices from the panel and click the button.")
           )),
           p("Note: Graphs may take a while to load. Thank you for your patience.")
           )
        ))
     ),
     tabPanel("Accessibility",
              value = "accTab",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectizeInput(inputId = "accDataQn",
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
                    ),
                    fluidRow(
                      column(
                        12,
                        actionButton("accButton", "Generate"), 
                        align = "center"
                      )
                    )
                  ),
                  mainPanel(
                    conditionalPanel(condition = "input.tabs == 'accTab' && input.accButton > 0", tmapOutput("accessibilityPlot",
                                                                                                             width = "100%",
                                                                                                             height = "80vh")),
                    conditionalPanel(condition = "input.tabs == 'accTab' && input.accButton == 0", column(width = 12,
                                                                                                          align="center",
                                                                                                          style = "height: 80vh; padding: 20px",
                                                                                                          h1("Please choose your choices from the panel and click the button.")
                    )),
                    p("Note: Graphs may take a while to load. Thank you for your patience.")
                  )
                  )
                )
              )
     )
  )