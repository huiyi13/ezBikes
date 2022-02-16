library(shiny)
library(shinyTime)
library(osrm)
# set profile to bike
options(osrm.profile = "bike")
library(leaflet)
library(raster)
library(spatialrisk)
library(tidyverse)
library(ggmap)
library(curl)
library(jsonlite)
library(stringr)
library(lubridate)
library(shinyalert)
library(shinythemes)
library(leaflet.extras)
library(tidyverse)
library(shinyjs)
library(fontawesome)
register_google(key = "AIzaSyAHdFzjl2vPMziMw9V5c6YF4H1npM99pW8", write = T)

#--------------------------------------- UI ---------------------------------------
ui <- navbarPage(
  HTML('<span style="vertical-align: top;font-size: 25px; padding-top:10px; font-weight:bold"> ezBikes </span>'),
  selected = "Cycling Route", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
  
  # -------------- Cycling Route tab --------------
  
  tabPanel("Cycling Route", icon=icon("bicycle"),
           
           fluidPage(
             theme = shinytheme("superhero"),
             
             headerPanel("Cycling Route"),
             
             sidebarPanel(
               h5("Please input your desired starting and ending locations in the form of an address, or building name, or street name, or postal code"
                  ,style="color:#def9ff"),
               helpText("(e.g. Mochtar Riady Building, Clementi MRT, Bishan Park etc.)"),
               
               
               tags$style("label{font-size: 16px;}"),
               textInput(inputId = "Startlocation",
                         label = "Starting Location:"),
               textInput(inputId = "Endlocation",
                         label = "Ending Location:"),
               br(),
               h5("Increase the radius of interest to view more amenities surrounding your route"
                  ,style="color:#def9ff"),
               
               sliderInput(inputId = "InterestRadius",
                           label = "Radius of Interest (m)",
                           value = 200,
                           min = 100,
                           max = 2000,
                           round = T),
               actionButton("submit", "Let's Go!", class = "btn-info", icon=icon("bicycle")),
               
               h4('Estimated duration of trip (mins):'),
               textOutput("Duration"),
               
               h4('Estimated distance of trip (km):'),
               textOutput("Distance")
             ),
             
             mainPanel(
               leafletOutput("pathPlot"),
               
               helpText("Tip: Select a marker on the map to view info below", style ="color:#c4c4c4"),
               
               
               wellPanel(
                 tableOutput("Information"))
               
             ))),
  
  # -------------- Weather tab --------------
  
  tabPanel("Weather", icon=icon("cloud"),
           fluidPage(
             sidebarPanel(
               h5("Please select any date to view the next 3 days of weather", style="color:#def9ff"),
               
               dateInput("date1", "Date:", value = Sys.Date()),
               
               em(helpText("*If the app disconnects after a date is selected, 
                        this date is unavailable in our database. Please select another date."))
             ),
             
             mainPanel(
               leafletOutput("weatherplot"),
               helpText("Tip: Showing next 3 days weather", style ="color:#c4c4c4"),
               
               fluidRow(
                 
                 column(3,
                        wellPanel(
                          div(textOutput("day1date"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                          br(),
                          div(uiOutput('day1icon'),style="font-size: 25px;text-align: center;") ,
                          br(),
                          div(textOutput("day1high"),style="font-size: 25px;font-weight: bold;  text-align: center;"),
                          div(textOutput("day1low"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                        )
                 ),
                 column(3,
                        wellPanel(
                          div(textOutput("day2date"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                          br(),
                          div(uiOutput('day2icon'),style="font-size: 25px;text-align: center;") ,
                          br(),
                          div(textOutput("day2high"),style="font-size: 25px;font-weight: bold;  text-align: center;"),
                          div(textOutput("day2low"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                          
                        )
                 ),
                 column(3,
                        wellPanel(
                          div(textOutput("day3date"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                          br(),
                          div(uiOutput('day3icon'),style="font-size: 25px;text-align: center;") ,
                          br(),
                          div(textOutput("day3high"),style="font-size: 25px;font-weight: bold;  text-align: center;"),
                          div(textOutput("day3low"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                        )
                 ),
                 column(3,
                        wellPanel(
                          div(textOutput("day4date"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                          br(),
                          div(uiOutput('day4icon'),style="font-size: 25px;text-align: center;") ,
                          br(),
                          div(textOutput("day4high"),style="font-size: 25px;font-weight: bold;  text-align: center;"),
                          div(textOutput("day4low"),style="font-size: 25px;font-weight: bold;  text-align: center;color:  #D3CBC9;"),
                        )
                 ),
                 
               )
               
             )
           )
  ),
  
  # -------------- About tab --------------
  
  tabPanel("About", icon=icon("info-circle"),
           fluidPage(
             div(img(src="cycling_singapore.png", height = 300), style="text-align: center;"),
             
             h1("Background", style="color:#00e5ff"),
             
             p("In 2015, the Sustainable Singapore Blueprint revealed a detailed 15-year plan for Singapore to move towards 
             being a 'car-lite' city. It aims to reduce reliance on motorised vehicles and decrease carbon emissions by sh
             ifting the default choices for transport to public transport, walking and cycling. To achieve this, 
             the Singaporean government will introduce innovative features and designs across the region to develop a better 
             and more comprehensive cycling network, spanning more than 1,320km by 2030. This has encouraged more 
             people to use bicycles as a form of transportation. With COVID-19, the demand for cycling has also increased 
             exponentially. At the height of the pandemic, many people took up cycling as a means of exercising amid the 
             restrictions. The sale of bicycles and cycling gear at Decathlon was reported to have doubled during the pandemic 
             as compared to 2020. Even bike-sharing operators such as Anywheel have seen an increase in its ridership 
             for every month last year."),
             
             p("For veteran and beginner cyclists alike, route planning is essential and especially so for longer travel 
               distances. In the current market for cycle route planning apps, there are options that offer the basic 
               service of route planning such as Bikemap, Strava, Cyclers Navigate and Google Maps. However, there is no 
               readily accessible, comprehensive app on the market that provides additional features to enhance the rider's 
               journey. These apps typically only help to plan routes without accounting for potential obstacles 
               like inclement weather which may affect the cyclist's journey. Furthermore, most of these apps are 
               created by companies based overseas and hence may not be localised enough to a Singaporean setting."),
             
             h1("ezBikes", style="color:#00e5ff"),
             
             h4("To address these gaps in the current market, the aim of ezBikes is to be an all-in-one app that 
             allows cyclists to conveniently map a detailed cycling route to their desired location. The ezBikes 
             app proactively offers value-adding features to make the journey more comfortable such as the weather
             forecast, and various pinned locations en route for food options, medical attention or toilet breaks 
             as needed. In the internet age, users are often bombarded with a deluge of information. Hence, ezBikes 
             is tailored to show only relevant information such as nearby points of interest specific to
             their cycling journey.")
             
           )
  )
  
)


#--------------------------------------- Server ---------------------------------------
server <- function(input, output){
  
  # -------------- Cycling Route tab --------------
  
  #functions to convert input addresses to lat long
  searchbar1 <- reactive(
    {
      location <- paste0(input$Startlocation,",Singapore")
      
      coords <- geocode(location)
      
      return(coords)
    })
  
  searchbar2 <- reactive(
    {
      location <- paste0(input$Endlocation,",Singapore")
      
      coords <- geocode(location)
      return(coords)
    })
  
  pcn <- read.csv("./Data files/pcn_access_points.csv")
  bike_racks <- read.csv("./Data files/bike_racks.csv")
  clinics <- read.csv("./Data files/clinics.csv")
  supermarkets <- read.csv("./Data files/supermarkets.csv")
  toilets <- read.csv("./Data files/toilets.csv")
  
  # Live weather data
  date <- Sys.Date() # returns current yyyy-mm-dd
  weather_url <- paste0("https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date=", date)
  weather_data <- fromJSON(weather_url)
  weather_info <- as.data.frame(weather_data$area_metadata) 
  weather_area <- weather_info$name
  weather_latlong <- weather_info$label_location
  weather_locations <- cbind(weather_area, weather_latlong)
  names(weather_locations) <- c("area", "lat", "long")
  weather_time <- tail(weather_data$items, 1)$update_timestamp
  weather_time_start <- sub("T", " ", tail(weather_data$items, 1)$valid_period$start)
  i <- unlist(gregexpr("\\+", weather_time_start))
  weather_time_start <- strptime(weather_time_start, format="%Y-%m-%d %H:%M:%S")
  weather_time_end <- sub("T", " ", tail(weather_data$items, 1)$valid_period$end)
  weather_time_end <- strptime(weather_time_end, format="%Y-%m-%d %H:%M:%S")
  weather_forecast <- tail(weather_data$items, 1)$forecasts
  weather <- merge(weather_forecast, weather_locations, by="area")
  weather$time_start <- weather_time_start
  weather$time_end <- weather_time_end
  
  # making icons
  pcn_icon <- makeIcon(
    iconUrl = "./Icons/pcn_icon.png",iconWidth = 35, iconHeight = 35)
  
  bikerack_icon <- makeIcon(
    iconUrl = "./Icons/bike_rack_icon.png",
    iconWidth = 35, iconHeight = 35)
  
  clinic_icon <- makeIcon(
    iconUrl = "./Icons/clinic_icon.png",
    iconWidth = 35, iconHeight = 35)
  
  supermarket_icon <- makeIcon(
    iconUrl = "./Icons/supermarket_icon.png",
    iconWidth = 35, iconHeight = 35)
  
  toilet_icon <- makeIcon(
    iconUrl = "./Icons/toilet_icon.png",
    iconWidth = 35, iconHeight = 35)
  
  flag_icon <- makeIcon(
    iconUrl = "./Icons/flag_icon.png",
    iconWidth = 35, iconHeight = 35)
  
  weather_icon <- makeIcon(
    iconUrl = "./Icons/cloudy.png",
    iconWidth = 35, iconHeight = 35)
  
  # initialize duration and distance to nothing before first route is entered
  duration_code <- 0
  distance_code <- 0
  
  # plots the route and returns points along the way. (to find nearby pins of interest)
  plotRoute <- function(src, dst, rad) {
    
    ### fetches route points, estimated duration and distance
    routeInfo <- osrmRoute(src, dst, overview = "full", returnclass = "sf")
    
    ## can change how many decimal places to show
    duration_code <<- round(routeInfo$duration, digits = 1)
    distance_code <<- round(routeInfo$distance, digits = 1)
    
    r <- as.data.frame(unclass(routeInfo$geometry[[1]]))
    colnames(r) <- c("lon", "lat")
    
    num_representative_pts = distance_code * 1000 / rad
    sample_indices <- unique(floor(seq(from = 1, to = nrow(r), length.out = num_representative_pts)))
    representative_pts <- r[sample_indices,]
    
    # upon app startup, there is no route, so there are no representative points
    # use the entire route r only for first iteration
    if (nrow(representative_pts) <= 0) {
      representative_pts <- r
    }
    
    pcn_points <- data.frame()
    bike_racks_points <- data.frame()
    clinics_points <- data.frame()
    supermarkets_points <- data.frame()
    toilets_points <- data.frame()
    weather_points <- data.frame()
    
    for (i in 1:nrow(representative_pts))
    {
      # filter points within chosen radius
      points1 <- points_in_circle(pcn, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = longitude, lat=latitude)
      pcn_points <- bind_rows(pcn_points, points1)
      
      points2 <- points_in_circle(bike_racks, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = longitude, lat=latitude)
      bike_racks_points <- bind_rows(bike_racks_points, points2)
      
      points3 <- points_in_circle(clinics, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = longitude, lat=latitude)
      clinics_points <- bind_rows(clinics_points, points3)
      
      points4 <- points_in_circle(supermarkets, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = long, lat=lat)
      supermarkets_points <- bind_rows(supermarkets_points, points4)
      
      points5 <- points_in_circle(toilets, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = long, lat=lat)
      toilets_points <- bind_rows(toilets_points, points5)
      
      points6 <- points_in_circle(weather, representative_pts$lon[i], representative_pts$lat[i], radius = rad, lon = long, lat=lat)
      weather_points <- bind_rows(weather_points, points6)
    }
    
    # plot map
    m <- leaflet() %>% addTiles(group = "Default") %>% addPolylines(data = r, 
                                                                    lat = ~lat, 
                                                                    lng = ~lon,
                                                                    color = 'black',
                                                                    weight = 3,
                                                                    opacity = 1) %>% 
      
      addMarkers(data=src, lat = ~lat, lng = ~lon, popup = paste0('<strong>',"Start: ",'</strong>',input$Startlocation)) %>%
      addMarkers(data=dst, lat = ~lat, lng = ~lon, popup = paste0("End: ",input$Endlocation), icon=flag_icon) %>%
      
      # mark out PCN access points within chosen radius
      addMarkers(data=pcn_points, lat = ~latitude, lng = ~longitude, popup = ~paste0('<strong>',Name, '</strong>',"<br/>", Location), 
                 icon = pcn_icon, group="PCN Access Points", layerId = ~paste("[Name:]",Name,'%', "[Location:]", Location, '%',
                                                                              "[Description:]",Description, '%',
                                                                              "[More Info:]",More.Info, '%',
                                                                              "[Length of Park Connector:]",Length.of.PC, '%',
                                                                              "[Opening Hours:]",Opening.Hours,'%',
                                                                              "[Lighting Hours:]",Lighting.Hours,'%',
                                                                              "[Accessibility:]",Accessibility)) %>%
      
      
      # mark out bike racks within chosen radius
      addMarkers(data=bike_racks_points, lat = ~latitude, lng = ~longitude, popup = ~paste0(Name,"<br/>", "total no.of racks: ",rack_count), 
                 icon = bikerack_icon, group="Bike racks", layerId = ~paste("[Name:]",Name, '%',"[Rack Count:]",rack_count,'%', 
                                                                            "[Address:]",address)) %>%
      
      # mark out clinics within chosen radius
      addMarkers(data=clinics_points, lat = ~latitude, lng = ~longitude, popup = ~clinic_names, icon = clinic_icon, group="Clinics",
                 layerId = ~paste("[Clinic Name:]",clinic_names, '%' , "[Address:]", address)) %>%
      
      # mark out supermarkets within chosen radius
      addMarkers(data=supermarkets_points, lat = ~lat, lng = ~long, popup = ~business_name,icon = supermarket_icon, group="Supermarkets", 
                 layerId = ~paste("[Licence Number:]", licence_numbers, '%', "[Business Name:]",business_name, '%',
                                  "[Address:]", premise_address)) %>%
      
      # mark out toilets within chosen radius
      addMarkers(data=toilets_points, lat = ~lat, lng = ~long, popup = ~Name, icon=toilet_icon, group="Toilets", 
                 layerId = ~paste("[Location Type:]",Type,'%', "[Location:]",Name,'%', "[Address:]",Address,'%',"[Region:]",Region)) %>%
      
      # mark out weather within chosen radius
      addMarkers(data=weather_points, lat = ~lat, lng = ~long, popup = ~forecast, icon=weather_icon, group="Weather", 
                 layerId = ~paste("[Area:]",area, '%', "[Forecast:]",forecast)) %>%
      
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      
      addControlGPS(
        options = gpsOptions(
          position = "topleft",
          activate = TRUE, 
          autoCenter = TRUE,
          setView = TRUE)) %>%
      
      addLayersControl(
        baseGroups = c("Default", "Satellite"),
        overlayGroups = c("PCN Access Points", "Bike racks", "Clinics", "Supermarkets", "Toilets", "Weather"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    activateGPS(m)
    return(m)
  }
  
  # -------------- Weather tab --------------
  
  
  ## 4 days weather data
  
  getWeather_by_date <- function(date) {
    # returns current yyyy-mm-dd
    
    weather_url <- paste0("https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date=", date)
    weather_data <- fromJSON(weather_url)
    weather_info <- as.data.frame(weather_data$area_metadata) 
    weather_area <- weather_info$name
    weather_latlong <- weather_info$label_location
    weather_locations <- cbind(weather_area, weather_latlong)
    names(weather_locations) <- c("area", "lat", "long")
    weather_time <- tail(weather_data$items, 1)$update_timestamp
    weather_time_start <- sub("T", " ", tail(weather_data$items, 1)$valid_period$start)
    i <- unlist(gregexpr("\\+", weather_time_start))
    weather_time_start <- strptime(weather_time_start, format="%Y-%m-%d %H:%M:%S")
    weather_time_end <- sub("T", " ", tail(weather_data$items, 1)$valid_period$end)
    weather_time_end <- strptime(weather_time_end, format="%Y-%m-%d %H:%M:%S")
    weather_forecast <- tail(weather_data$items, 1)$forecasts
    weather <- merge(weather_forecast, weather_locations, by="area")
    weather$time_start <- weather_time_start
    weather$time_end <- weather_time_end
    
    return ( weather)
  }
  
  getTemperature_by_date <- function(date) {
    # returns current yyyy-mm-dd
    date = date  - 1
    
    weather_url <- paste0("https://api.data.gov.sg/v1/environment/4-day-weather-forecast?date=", date)
    weather_data <- fromJSON(weather_url)
    weather_info <- as.data.frame(weather_data$items$forecasts) 
    date <- weather_info$date
    low <- weather_info$temperature$low
    high <- weather_info$temperature$high
    forcast <- weather_info$forecast
    
    return (data.frame(date = date,low = low , high = high , forcast = forcast));
    
  }
  
  observeEvent(input$date1,{
    
    temperature = getTemperature_by_date(input$date1)
    
    output$day1icon <- renderUI({
      icon("cloud-rain")
    })
    output$day2icon <- renderUI({
      icon("cloud-sun")
    })
    output$day3icon <- renderUI({
      icon("cloud")
    })
    output$day4icon <- renderUI({
      icon("cloud-sun-rain")
    })
    output$day1date <- renderText({
      as.character( temperature$date[1])
    })
    output$day2date <- renderText({
      as.character( temperature$date[2])
    })
    output$day3date <- renderText({
      as.character( temperature$date[3])
    })
    output$day4date <- renderText({
      as.character( temperature$date[4])
    })
    
    output$day1high <-renderText({
      temperature$high[1]
    })
    output$day1low <-renderText({
      temperature$low[1]
    })
    output$day2high <-renderText({
      temperature$high[2]
    })
    output$day2low <-renderText({
      temperature$low[2]
    })
    output$day3high <-renderText({
      temperature$high[3]
    })
    output$day3low <-renderText({
      temperature$low[3]
    })
    output$day4high <-renderText({
      temperature$high[4]
    })
    output$day4low <-renderText({
      temperature$low[4]
    })
    
    
    req(input$date1)
    currentdate <- as.POSIXct(Sys.Date(), format = "%Y-%m-%d")
    
    date1 <- as.POSIXct(as.Date( input$date1)+1 , format = "%Y-%m-%d")
    date2 <- as.POSIXct(as.Date( input$date1)+2 , format = "%Y-%m-%d")
    date3 <- as.POSIXct(as.Date( input$date1)+3 , format = "%Y-%m-%d")
    datecheck <- as.POSIXct(as.Date( input$date1) , format = "%Y-%m-%d")
    
    
    weather = data.frame();
    weather1 = data.frame();
    weather2 = data.frame();
    weather3 = data.frame();
    
    
    if ( currentdate == date3  ){
      print("three")
      
      dates = as.Date(input$date1 )
      
      weather= getWeather_by_date( dates)
      weather$icon = weather$forecast
      dates1 = as.Date(input$date1 ) + 1
      weather1= getWeather_by_date(dates1)
      
      dates2 = as.Date(input$date1 ) + 2
      weather2= getWeather_by_date(dates2)
      
      dates3 = as.Date(input$date1 ) + 3
      weather3= getWeather_by_date(dates3)
      weather$forecast =  paste(dates ,"&emsp;",weather$forecast ,"<br>",dates1 ,"&emsp;", weather1$forecast,"<br>",dates2 ,"&emsp;",weather2$forecast,"<br>",dates3 ,"&emsp;",weather3$forecast)
      
      
      
    }else if(currentdate == date2  ){
      print("two")
      dates = as.Date(input$date1 )
      
      weather= getWeather_by_date( dates)
      weather$icon = weather$forecast
      
      dates1= as.Date(input$date1 ) + 1
      weather1= getWeather_by_date(dates1)
      
      dates2 = as.Date(input$date1 ) + 2
      weather2= getWeather_by_date(dates2)
      weather$forecast =  paste(dates ,"&emsp;",weather$forecast ,"<br>",dates1 ,"&emsp;", weather1$forecast,"<br>",dates2 ,"&emsp;",weather2$forecast)
      
      
      
    }else if(currentdate == date1  ){
      print("one")
      dates = as.Date(input$date1 )
      
      weather= getWeather_by_date( dates)
      weather$icon = weather$forecast
      
      dates1 = as.Date(input$date1 ) + 1
      weather1= getWeather_by_date(dates1)
      weather$forecast =  paste(dates ,"&emsp;",weather$forecast ,"<br>",dates1 ,"&emsp;", weather1$forecast)
      
      
      
    }else if(datecheck == currentdate){
      print("today")
      dates = as.Date(input$date1 )
      
      weather= getWeather_by_date( dates)
      weather$icon = weather$forecast
      weather$forecast =  paste(dates ,"&emsp;",weather$forecast )
      
      
    }else if(currentdate < datecheck ){
      print("tommorow")
      return();
    }else{
      print("any")
      dates = as.Date(input$date1 )
      
      weather= getWeather_by_date( dates)
      weather$icon = weather$forecast
      
      dates1 = as.Date(input$date1 ) + 1
      weather1= getWeather_by_date(dates1)
      
      dates2 = as.Date(input$date1 ) + 2
      weather2= getWeather_by_date(dates2)
      
      dates3 = as.Date(input$date1 ) + 3
      weather3= getWeather_by_date(dates3)
      weather$forecast =  paste(dates ,"&emsp;",weather$forecast ,"<br>",dates1 ,"&emsp;", weather1$forecast,"<br>",dates2 ,"&emsp;",weather2$forecast,"<br>",dates3 ,"&emsp;",weather3$forecast)
      
      
    }
    
    weather_icon <- makeIcon(
      iconUrl = "./Icons/cloudy.png",
      iconWidth = 35, iconHeight = 35)
    Partly_Cloudy_weather_icon <- makeIcon(
      iconUrl = "./Icons/Partly_Cloudy.png",
      iconWidth = 35, iconHeight = 35)
    
    
    
    weather= mutate(weather ,icon = str_replace(icon, " ", "_"))
    
    output$weatherplot <- renderLeaflet({
      
      
      
      # logos for each weather type
      logos <- awesomeIconList(
        "Light_Rain" = makeAwesomeIcon(
          
          text = fa("cloud-rain"),
          markerColor = "lightred",
        ),
        "Partly_Cloudy (Night)" = makeAwesomeIcon(
          text = fa("cloud-sun"),
          markerColor = "lightgreen",
        ),
        "Cloudy" = makeAwesomeIcon(
          text = fa("cloud"),
          markerColor = "green"
          
        ),
        "Moderate_Rain" = makeAwesomeIcon(
          
          text = fa("cloud-sun-rain"),
          markerColor = "red",
          
        )
      )
      
      leaflet() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 10) %>%
        addTiles() %>%
        addAwesomeMarkers(data = weather , lng = ~long , lat = ~lat , popup = paste(weather$area,"<br><br>",weather$forecast),icon=~logos[icon])
      
    })
  })
  
  
  
  output$pathPlot = renderLeaflet({
    output$Duration <- renderPrint({duration_code})
    output$Distance <- renderPrint({distance_code})
    input$submit
    
    isolate({plotRoute(searchbar1(), searchbar2(), input$InterestRadius)})})
  #isolate prevents leaflet map from refreshing everytime the input changes,
  #ensuring that map will only change after submit button is clicked on
  
  observeEvent(input$pathPlot_marker_click,{ 
    event <- input$pathPlot_marker_click
    info_names <- do.call(rbind, str_extract_all(string = event$id,pattern = "(?<=\\[).+?(?=\\])"))
    
    event$id <- gsub("\\[.*?\\]","",event$id, ignore.case = FALSE, fixed = FALSE)
    df <- as.data.frame(strsplit(event$id, '%'))
    
    rownames(df) <- info_names
    names(df) <- event$group
    
    output$Information <- renderTable({df}, include.rownames=TRUE)
    
  })
  
}

shinyApp(ui,server)

