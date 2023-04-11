pacman::p_load(shiny, shinyWidgets, sf, tmap, SpatialAcc, tidyverse, spatstat)


# Geospatial
mpsz_original <- read_rds("data/models/mpsz_original.rds")


## Models
total_pop <- read_rds("data/models/total_pop.rds")

gp_sf <- readRDS(file = "data/models/sf/gp_sf.rds")
hospital_sf <- readRDS(file = "data/models/sf/hospital_sf.rds")
nursing_sf <- readRDS(file = "data/models/sf/nursing_sf.rds")
poly_sf <- readRDS(file = "data/models/sf/poly_sf.rds")
chas_sf <- readRDS(file = "data/models/sf/chas_sf.rds")
pcn_sf <- readRDS(file = "data/models/sf/pcn_sf.rds")

gp_ppp.km <- readRDS(file = "data/models/ppp_km/gp_ppp_km.rds")
hospital_ppp.km <- readRDS(file = "data/models/ppp_km/hospital_ppp_km.rds")
nursing_ppp.km <- readRDS(file = "data/models/ppp_km/nursing_ppp_km.rds")
poly_ppp.km <- readRDS(file = "data/models/ppp_km/poly_ppp_km.rds")
chas_ppp.km <- readRDS(file = "data/models/ppp_km/chas_ppp_km.rds")
pcn_ppp.km <- readRDS(file = "data/models/ppp_km/pcn_ppp_km.rds")

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
    
    output$aspatialDataPlot <- renderTmap({

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
      
      tmap_mode("plot")
      tmap_options(check.and.fix = TRUE) +
      tm_shape(total_pop) +
        tm_polygons(
          "Total...2",
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
    
    
    output$KDEDataPlot <- renderPlot({
      # KDE
      
      # data
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
    
    
    output$accessibilityPlot <- renderTmap({
      
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
        distmat_data <- distmat_PCN_Clinics_km
        hexagon_spec <- hexagons_2019
      }
      else if(input$accDataQn == "CHAS Clinics"){
        accData <- chas_data
        accDemand <- chas_demand
        distmat_data <- distmat_chas_km
        hexagon_spec <- hexagons_2019
      } 
      else if(input$accDataQn == "Eldercare"){
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
}
