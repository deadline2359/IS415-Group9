pacman::p_load(shiny, shinyWidgets, readr, sf, vctrs, tmap, spatstat, sfdep, tidyr, tidyverse, maptools)


# Aspatial
pop_data <- readr::read_csv("data/aspatial/Resident Population 2015.csv", skip=11)[1:379,1:19]
gp_data <- readr::read_csv("data/aspatial/gp_data_geocoded.csv")[,-1]
hospital_data <- readr::read_csv("data/aspatial/hospital_data_geocoded.csv")
poly_data <- readr::read_csv("data/aspatial/polyclinic_data_geocoded.csv")
nursing_data <- readr::read_csv("data/aspatial/nursing_home_data_geocoded.csv")
pcn_data <- readr::read_csv("data/aspatial/PCN Clinic Listing (by PCN) With Postal Code.csv")




# Geospatial
mpsz_original <- st_read(dsn = "data/geospatial/MPSZ-2019",
                layer = "MPSZ-2019") %>%
  st_transform(crs = 3414)
mpsz <- st_make_valid(mpsz_original)

chas_sf <- st_read(dsn = "data/geospatial/CHAS Clinics Shapefile",
                   layer = "CHAS Clinics") %>%
  st_transform(crs = 3414)

pcn_data <- pcn_data[rowSums(is.na(pcn_data)) == 0, ]
pcn_sf <- st_as_sf(pcn_data, coords=c("results.LONGITUDE", "results.LATITUDE"), crs=4326) %>% st_transform(crs = 3414)




# Data Preparation

## CHAS - remove rows with NAs
chas_sf <- chas_sf[rowSums(is.na(chas_sf)) == 0, ]


## Retrieve Geospatial Data
gp_sf <- st_as_sf(gp_data, coords=c("Long", "Lat"), crs=4326) %>% st_transform(crs = 3414)
hospital_sf <- st_as_sf(hospital_data, coords=c("Long", "Lat"), crs=4326) %>% st_transform(crs = 3414)
poly_sf <- st_as_sf(poly_data, coords=c("Long", "Lat"), crs=4326) %>% st_transform(crs = 3414)
nursing_sf <- st_as_sf(nursing_data, coords=c("Long", "Lat"), crs=4326) %>% st_transform(crs = 3414)


## Excluding Unnecessary Data Points
gp_sf <- st_intersection(mpsz, gp_sf)
hospital_sf <- st_intersection(mpsz, hospital_sf)
poly_sf <- st_intersection(mpsz, poly_sf)
nursing_sf <- st_intersection(mpsz, nursing_sf)
pcn_sf <- st_intersection(mpsz, pcn_sf)
chas_sf <- st_intersection(mpsz, chas_sf)


## Merge MPSZ with Population Data
### Convert Data Types
pop_is_char <- sapply(pop_data[c(2:19)], is.character)
pop_data[c(2:19)][ , pop_is_char] <- as.data.frame(apply(pop_data[c(2:19)][ , pop_is_char], 2, as.numeric))

### Total Population
pop_data$...1  = toupper(pop_data$...1)
total_pop <- merge(x = mpsz, y = pop_data, by.x = "SUBZONE_N", by.y = "...1", all.x = TRUE)







function(input, output, session) {
    # aspatialDataInput <- reactive({
    #   switch(input$aspatialDataQn, 
    #          "Resident Population" = aspatialData$dataChosen
    #          )
    # })
    
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
          alpha = 0.4) +
        tm_fill() +
        tm_shape(aspatialDataChosen) +
        tm_dots(col  = "blue",
               size = 0.05) +
        tm_view(set.zoom.limits = c(11,14),
                set.view = 11,
                set.bounds = TRUE)
    })
    
    
    
    
    
    output$KDEDataPlot <- renderPlot({
      # KDE
      ## Converting sf data frames to sp's Spatial class
      mpsz_spatial <- as_Spatial(mpsz_original)
      gp_spatial <- as_Spatial(gp_sf)
      hospital_spatial <- as_Spatial(hospital_sf)
      poly_spatial <- as_Spatial(poly_sf)
      nursing_spatial <- as_Spatial(nursing_sf)
      chas_spatial <- as_Spatial(chas_sf)
      pcn_spatial <- as_Spatial(pcn_sf)


      ## Converting sp's *Spatial** Class into Generic sp Format
      mpsz_sp <- as(mpsz_spatial, "SpatialPolygons")
      gp_sp <- as(gp_spatial, "SpatialPoints")
      hospital_sp <- as(hospital_spatial, "SpatialPoints")
      poly_sp <- as(poly_spatial, "SpatialPoints")
      nursing_sp <- as(nursing_spatial, "SpatialPoints")
      chas_sp <- as(chas_spatial, "SpatialPoints")
      pcn_sp <- as(pcn_spatial, "SpatialPoints")

      ## Converting Generic sp Format into spatstat's ppp Format
      gp_ppp <- as(gp_sp, "ppp")
      hospital_ppp <- as(hospital_sp, "ppp")
      poly_ppp <- as(poly_sp, "ppp")
      nursing_ppp <- as(nursing_sp, "ppp")
      chas_ppp <- as(chas_sp, "ppp")
      pcn_ppp <- as(pcn_sp, "ppp")

      ## Duplicated Points
      gp_ppp_jit <- rjitter(gp_ppp,retry = TRUE,
                            nsim = 1,
                            drop = TRUE)

      nursing_ppp_jit <- rjitter(nursing_ppp,retry = TRUE,
                                 nsim = 1,
                                 drop = TRUE)

      chas_ppp_jit <- rjitter(chas_ppp,retry = TRUE,
                              nsim = 1,
                              drop = TRUE)

      mpsz_owin <- as(mpsz_sp, "owin")
      gp_ppp = gp_ppp_jit[mpsz_owin]
      hospital_ppp = hospital_ppp[mpsz_owin]
      poly_ppp = poly_ppp[mpsz_owin]
      nursing_ppp = nursing_ppp_jit[mpsz_owin]
      chas_ppp = chas_ppp_jit[mpsz_owin]
      pcn_ppp = pcn_ppp[mpsz_owin]

      gp_ppp.km <- rescale(gp_ppp, 1000, "km")
      hospital_ppp.km <- rescale(hospital_ppp, 1000, "km")
      poly_ppp.km <- rescale(poly_ppp, 1000, "km")
      nursing_ppp.km <- rescale(nursing_ppp, 1000, "km")
      chas_ppp.km <- rescale(chas_ppp, 1000, "km")
      pcn_ppp.km <- rescale(pcn_ppp, 1000, "km")
      
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
      else if(input$KDEQn == "Bus Stops"){
        pppChosen <- busstop_ppp.km
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
}
