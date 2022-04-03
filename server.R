## server.R ##
library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(leaflet)
library(raster)
library(htmltools)
library(dplyr)


server <- function(input, output, session) 
{
  createContent <- function(i){
    name <- paste("<b>", power_station_coords$Station[i], "</b>", sep="")
    latitude <- paste("Latitude: ", round(power_station_coords$Latitude[i], digits = 2), sep="")
    longitude <- paste("Longitude: ", round(power_station_coords$Longitude[i], digits = 2), sep="")
    avg_month_coal <- paste("Average Monthly Coal Consumption (in 1000 tonnes): ", round(power_station_coords$month_coal[i], digits = 2), sep="")
    avg_month_prod <- paste("Average Monthly Production (in MU): ", round(power_station_coords$month_prod[i], digits = 2), sep="")
    
    
    content <- paste(sep = "<br/>",
                     name,
                     latitude, 
                     longitude, 
                     avg_month_coal,
                     avg_month_prod)
    
    power_station_coords$content[i] = content
    
    
  }
  
  alloc <- function(num_panels = 100000, equity = 0, region = "all"){
    final_mev <- read_excel("final_mev_india.xlsx", col_names=TRUE)
    lambda1 = 72.93
    lambda2 = 0.774463
    lambda3 = 30862
    lambda = 54.2
    num_panels = as.integer(num_panels)
    if(region == "all"){
      data = data.frame(final_mev)
    }
    else{
      print(region)
      data = data.frame(final_mev[(final_mev$region %in% region) == TRUE,])
    }
    i = num_panels
    df = data
    if(equity == "Equitable"){
      print("Allocating equitably...")
      i = (num_panels)/2
      df$panels = i/nrow(df)
      while(i > 0) {
        df = df[order(-df$MEV),]
        #print(df)
        df$MEV[1] = df$MEV[1] - lambda
        df$panels[1] = df$panels[1] + 1
        i = i - 1
      }
    }
    else if(equity == "Efficient"){
      print("Allocating efficiently...")
      df$panels = 0
      while(i > 0) {
        df = df[order(-df$MEV),]
        #print(df)
        df$MEV[1] = df$MEV[1] - lambda
        df$panels[1] = df$panels[1] + 1
        i = i - 1
      }
    }
    print("Allocation complete!")
    return(df)
  }
  
  geoData <- readLines("Indian_States.json") %>% paste(collapse = "\n") #India State Map
  
  power_station_coords <- read_excel("Power Station Coordinates.xlsx") #Power Station coordinates
  month_coal_file <- read_excel("month_coal.xlsx")
  month_coal_file$avg_month_coal = (rowSums(month_coal_file[,c(3,4,5,6,7,8,9,10,11,12,13,14)]))/12
  month_prod_file <- read_excel("station_monthly.xlsx")
  power_station_coords$affected_radius = 0
  power_station_coords$content = NA
  power_station_coords$month_coal = NaN
  power_station_coords$month_prod =  NaN
  
  cluster_details <- read_excel("clusters_data_1.xlsx")
  
  for (i in 1:nrow(power_station_coords)){
    
    station_name = power_station_coords$Station[i]
    
    month_coal_station = month_coal_file %>%
      filter(Station == station_name)
    
    if(nrow(month_coal_station) > 0){
      power_station_coords$month_coal[i] = month_coal_station$avg_month_coal[1]
    }
    
    month_prod_station = month_prod_file %>%
      filter(Station == station_name)
    
    power_station_coords$month_prod[i] = mean(month_prod_station$Daily_Production)
    
    power_station_coords$content[i] = createContent(i)
    
    
    station_cluster <- cluster_details %>%
      filter(`plant assigned` == station_name)
    
    if(nrow(station_cluster) != 0){
      power_station_coords$affected_radius[i] = max(station_cluster$`minimum distance`, na.rm = TRUE)
    }
    
  }
  
  power_station_coords_non_zero = power_station_coords %>%
    filter(affected_radius > 2)
  
  pvout <- raster("PVOUT.tif")
  
  pal <- colorNumeric(c("#e93e3a", "#f3903f", "#fff33b"), values(pvout),
                      na.color = "transparent")
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(dragging = FALSE)) %>% setView(lng = 78.963, lat = 20.594, zoom = 5.25) %>%
      addTiles(group = "World View") %>%
    addGeoJSON(geoData, weight = 1, color = "#444444", fill = FALSE, group = "India View")%>%
      addCircleMarkers(lng = power_station_coords$Longitude, lat = power_station_coords$Latitude, 
                       popup = power_station_coords$content, group = "Power Stations", 
                       radius = 2, opacity = 1, fillOpacity = 1, color = 'black') %>%
      addCircleMarkers(lng = power_station_coords_non_zero$Longitude, lat = power_station_coords_non_zero$Latitude, 
                       group = "Power Stations", 
                       radius = power_station_coords_non_zero$affected_radius, opacity = 0.5, color = 'red', 
                       fill = FALSE) %>%
      addLayersControl(
        baseGroups = c("World View", "India View"),
        overlayGroups = c("Power Stations"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  #select region
  observe({
    
    if("Select All" %in% input$myselect)
      selected_choices = append(choices[-1], "Eastern")
    else
      selected_choices= input$myselect
    
    updateSelectInput(session,"myselect", selected= selected_choices)
    })
  
  
  #Generate Optimal
  observeEvent(input$do, {
    
    sp = input$sp
    eqef = input$eqefchoice
    regions = input$myselect
    print(sp)
    print(eqef)
    if(length(regions)==4){
      regions = c("all")
    }
    print(regions)
    

    op = alloc(sp, eqef, regions)

    print(nrow(op))

    op = op %>%
      filter(panels != 0)

    print(nrow(op))


    leafletProxy("mymap") %>%
      clearShapes() %>%
      addCircles(lng = op$x, lat = op$y, radius = op$panels, label = htmlEscape(round(op$panels, digits = 2)))
  })
     
}

