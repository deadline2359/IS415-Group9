pacman::p_load(shiny, shinyWidgets, sf, tmap, SpatialAcc, tidyverse, spatstat, ggplot2, plotly)


# Geospatial
mpsz_original <- read_rds("data/models/mpsz_original.rds")


## Models
total_pop <- read_rds("data/models/pop/total_pop.rds")
male_pop <- read_rds("data/models/pop/male_pop.rds")
female_pop <- read_rds("data/models/pop/female_pop.rds")

gp_sf <- readRDS(file = "data/models/sf/gp_sf.rds")
hospital_sf <- readRDS(file = "data/models/sf/hospital_sf.rds")
nursing_sf <- readRDS(file = "data/models/sf/nursing_sf.rds")
poly_sf <- readRDS(file = "data/models/sf/poly_sf.rds")
chas_sf <- readRDS(file = "data/models/sf/chas_sf.rds")
pcn_sf <- readRDS(file = "data/models/sf/pcn_sf.rds")
eldercare_sf <- readRDS(file = "data/models/sf/eldercare_sf.rds")

gp_ppp.km <- readRDS(file = "data/models/ppp_km/gp_ppp_km.rds")
hospital_ppp.km <- readRDS(file = "data/models/ppp_km/hospital_ppp_km.rds")
nursing_ppp.km <- readRDS(file = "data/models/ppp_km/nursing_ppp_km.rds")
poly_ppp.km <- readRDS(file = "data/models/ppp_km/poly_ppp_km.rds")
chas_ppp.km <- readRDS(file = "data/models/ppp_km/chas_ppp_km.rds")
pcn_ppp.km <- readRDS(file = "data/models/ppp_km/pcn_ppp_km.rds")
eldercare_ppp.km <- readRDS(file = "data/models/ppp_km/eldercare_ppp_km.rds")

distmat_hospital_km <- readRDS(file = "data/models/distmat/distmat_Hospitals_km.rds")
distmat_nursing_km <- readRDS(file = "data/models/distmat/distmat_Nursing_Homes_km.rds")
distmat_poly_km <- readRDS(file = "data/models/distmat/distmat_Polyclinics_km.rds")
distmat_chas_km <- readRDS(file = "data/models/distmat/distmat_CHAS_Clinics_km.rds")
distmat_pcn_km <- readRDS(file = "data/models/distmat/distmat_PCN_Clinics_km.rds")
distmat_eldercare_km <- readRDS(file = "data/models/distmat/distmat_eldercare_km.rds")

hexagons <- read_rds("data/models/hexagons/hexagons.rds")
hexagons_2019 <- read_rds("data/models/hexagons/hexagons_2019.rds")

hospital_demand <- readRDS(file = "data/models/demand/Hospital.rds")
nursing_demand <- readRDS(file = "data/models/demand/Nursing_Homes.rds")
poly_demand <- readRDS(file = "data/models/demand/Polyclinics.rds")
chas_demand <- readRDS(file = "data/models/demand/CHAS_Clinics.rds")
pcn_demand <- readRDS(file = "data/models/demand/PCN_Clinics.rds")
eldercare_demand <- readRDS(file = "data/models/demand/eldercare.rds")

hospital_data <- readRDS(file = "data/models/accData/Hospital.rds")
nursing_data <- readRDS(file = "data/models/accData/Nursing_Homes.rds")
poly_data <- readRDS(file = "data/models/accData/Polyclinics.rds")
chas_data <- readRDS(file = "data/models/accData/CHAS_Clinics.rds")
pcn_data <- readRDS(file = "data/models/accData/PCN_Clinics.rds")
eldercare_data <- readRDS(file = "data/models/accData/eldercare.rds")


function(input, output, session) {
  
    # Aspatial
    aspatialDataInput <- eventReactive(input$aspatialButton, {
      if(input$aspatialDataQn == "General Practitioners (GPs)"){
        aspatialDataChosen <- gp_sf
      }
      else if(input$aspatialDataQn == "Hospitals"){
        aspatialDataChosen <- hospital_sf
      }
      else if(input$aspatialDataQn == "Polyclinics"){
        aspatialDataChosen <- poly_sf
      }
      else if(input$aspatialDataQn == "Nursing Homes"){
        aspatialDataChosen <- nursing_sf
      }
      else if(input$aspatialDataQn == "Primary Care Networks (PCN)"){
        aspatialDataChosen <- pcn_sf
      }
      else if(input$aspatialDataQn == "CHAS Clinics"){
        aspatialDataChosen <- chas_sf
      }
      else if(input$aspatialDataQn == "Eldercare Centres"){
        aspatialDataChosen <- eldercare_sf
      }
      
      if(input$aspatialSexQn == "Total Population"){
        aspatialPopChosen <- total_pop
      }
      else if(input$aspatialSexQn == "Male Population"){
        aspatialPopChosen <- male_pop
      }
      else if(input$aspatialSexQn == "Female Population"){
        aspatialPopChosen <- female_pop
      }
      
      tmap_mode("plot")
      tmap_options(check.and.fix = TRUE) +
        tm_shape(aspatialPopChosen) +
        tm_polygons(
          input$aspatialAgeQn,
          style = "cont",
          alpha = 0.8,
          palette = input$aspatialBGColourQn) +
        tm_fill() +
        tm_shape(aspatialDataChosen) +
        tm_dots(col  = input$aspatialSymbolColourQn,
                size = 0.05) +
        tm_view(set.zoom.limits = c(11,14),
                set.view = 11,
                set.bounds = TRUE)
    })
    
    output$aspatialDataPlot <- renderTmap({
      input$aspatialDataInput
      if (is.null(aspatialDataInput())){
        return(NULL)
      } else{
        # Progress Bar
        dat <- data.frame(x = numeric(0), y = numeric(0))
        withProgress(message = 'Generating...', value = 0, {
          n <- 10
          for (i in 1:n) {
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            incProgress(1/n, detail = paste("Loading part", i, "..."))
            Sys.sleep(0.1)
          }
          aspatialDataInput()
        })
      }
    })
  
  
    
    
    
  
    # KDE
    kdeDataInput <- eventReactive(input$KDEButton, {
      if(input$KDEQn == "General Practitioners (GPs)"){
        pppChosen <- gp_ppp.km
      }
      else if(input$KDEQn == "Hospitals"){
        pppChosen <- hospital_ppp.km
      }
      else if(input$KDEQn == "Polyclinics"){
        pppChosen <- poly_ppp.km
      }
      else if(input$KDEQn == "Nursing Homes"){
        pppChosen <- nursing_ppp.km
      }
      else if(input$KDEQn == "Primary Care Networks (PCN)"){
        pppChosen <- pcn_ppp.km
      }
      else if(input$KDEQn == "CHAS Clinics"){
        pppChosen <- chas_ppp.km
      }
      else if(input$KDEQn == "Eldercare Centres"){
        pppChosen <- eldercare_ppp.km
      }
      
      #bandwidth methods
      if(input$KDEBandwidthQn == "bw.diggle"){
        pppChosen_bw <- density(pppChosen,
                                sigma = bw.diggle,
                                edge = TRUE,
                                kernel = input$KDEKernelQn)
      }
      else if(input$KDEBandwidthQn == "bw.CvL"){
        pppChosen_bw <- density(pppChosen,
                                sigma = bw.CvL,
                                edge = TRUE,
                                kernel = input$KDEKernelQn)
      }
      else if(input$KDEBandwidthQn == "bw.scott"){
        pppChosen_bw <- density(pppChosen,
                                sigma = bw.scott,
                                edge = TRUE,
                                kernel = input$KDEKernelQn)
      }
      else if(input$KDEBandwidthQn == "bw.ppl"){
        pppChosen_bw <- density(pppChosen,
                                sigma = bw.ppl,
                                edge = TRUE,
                                kernel = input$KDEKernelQn)
      }
      
      plot(pppChosen_bw, main = input$KDEQn)
    })
    
    output$KDEDataPlot <- renderPlot({
      input$kdeDataInput
      if (is.null(kdeDataInput())){
        return(NULL)
      } else{
        # Progress Bar
        dat <- data.frame(x = numeric(0), y = numeric(0))
        withProgress(message = 'Generating...', value = 0, {
          n <- 10
          for (i in 1:n) {
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            incProgress(1/n, detail = paste("Loading part", i, "..."))
            Sys.sleep(0.1)
          }
          kdeDataInput()
        })
      }
  })
    
    
    
    
    
    
    # Accessibility Map
    accDataInput <- eventReactive(input$accButton, {
      if(input$accDataQn == "Hospitals"){
        accData <- hospital_data
        accDemand <- hospital_demand
        distmat_data <- distmat_hospital_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Polyclinics"){
        accData <- poly_data
        accDemand <- poly_demand
        distmat_data <- distmat_poly_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Nursing Homes"){
        accData <- nursing_data
        accDemand <- nursing_demand
        distmat_data <- distmat_nursing_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Primary Care Networks (PCN)"){
        accData <- pcn_data
        accDemand <- pcn_demand
        distmat_data <- distmat_pcn_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "CHAS Clinics"){
        accData <- chas_data
        accDemand <- chas_demand
        distmat_data <- distmat_chas_km
        hexagon_spec <- hexagons_2019
      } 
      else if(input$accDataQn == "Eldercare Centres"){
        accData <- eldercare_data
        accDemand <- eldercare_demand
        distmat_data <- distmat_eldercare_km
        hexagon_spec <- hexagons
      }
      
      acc_data_fun <- data.frame(ac(hexagon_spec$demand,
                                    accDemand$capacity,
                                    distmat_data, 
                                    d0 = 50,
                                    power = 0.5, 
                                    family = input$accFunQn))
      
      
      colnames(acc_data_fun) <- "accDataFun"
      acc_data_fun <- tibble::as_tibble(acc_data_fun)
      hexagon_data_fun <- bind_cols(hexagon_spec, acc_data_fun)
      
      mapex_2019 <- st_bbox(hexagon_spec)
      
      tm_shape(hexagon_data_fun,
               bbox = mapex_2019) +
        tm_fill(col = "accDataFun",
                n = 10,
                palette = input$accColourQn,
                style = "quantile",
                border.col = "black",
                border.lwd = 1) +
        tm_shape(accData) +
        tm_symbols(size = 0.1) +
        tm_view(set.zoom.limits = c(11,14),
                set.view = 11,
                set.bounds = TRUE)
    })
    
    output$accessibilityPlot <- renderTmap({
      input$accDataInput
      if (is.null(accDataInput())){
        return(NULL)
      } else{
        # Progress Bar
        dat <- data.frame(x = numeric(0), y = numeric(0))
        withProgress(message = 'Generating...', value = 0, {
          n <- 10
          for (i in 1:n) {
            dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
            incProgress(1/n, detail = paste("Loading part", i, "..."))
            Sys.sleep(0.1)
          }
          accDataInput()
        })
      }
    })
    
    
    # Accessibility Boxplot
    accBoxPlotDataInput <- eventReactive(input$accButton, {
      if(input$accDataQn == "Hospitals"){
        accData <- hospital_data
        accDemand <- hospital_demand
        distmat_data <- distmat_hospital_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Polyclinics"){
        accData <- poly_data
        accDemand <- poly_demand
        distmat_data <- distmat_poly_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Nursing Homes"){
        accData <- nursing_data
        accDemand <- nursing_demand
        distmat_data <- distmat_pcn_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "Primary Care Networks (PCN)"){
        accData <- pcn_data
        accDemand <- pcn_demand
        distmat_data <- distmat_pcn_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "CHAS Clinics"){
        accData <- chas_data
        accDemand <- chas_demand
        distmat_data <- distmat_chas_km
        hexagon_spec <- hexagons_2019
      } 
      else if(input$accDataQn == "Eldercare Centres"){
        accData <- eldercare_data
        accDemand <- eldercare_demand
        distmat_data <- distmat_eldercare_km
        hexagon_spec <- hexagons
      }
      
      acc_data_fun <- data.frame(ac(hexagon_spec$demand,
                                    accDemand$capacity,
                                    distmat_data, 
                                    d0 = 50,
                                    power = 0.5, 
                                    family = input$accFunQn))
      
      
      colnames(acc_data_fun) <- "accDataFun"
      acc_data_fun <- tibble::as_tibble(acc_data_fun)
      hexagon_data_fun <- bind_cols(hexagon_spec, acc_data_fun)
      
      
      boxPlot_data <- st_join(hexagon_data_fun, mpsz_original, 
                              join = st_intersects)
      
      ggplot(data=boxPlot_data, 
             aes(y = log(accDataFun), 
                 x= factor(REGION_N, level=c('WEST REGION', 'NORTH REGION', 'CENTRAL REGION', 'NORTH-EAST REGION', 'EAST REGION')))) +
        geom_boxplot() + 
        xlab("Region") + 
        ylab("Log(Accessibility)")
    })
    
    output$accessibilityBoxPlot <- renderPlotly({
      input$accBoxPlotDataInput
      if (is.null(accBoxPlotDataInput())){
        return(NULL)
      } else{
        accBoxPlotDataInput()
        
      }
    })
}
