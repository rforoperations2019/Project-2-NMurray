## app.R ##
library(shinydashboard)
library(httr) # http rest requests
library(jsonlite) # fromJSON
library(utils) # URLencode functions
library(rgdal) # readOGR
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(rgeos)
library(shinyWidgets)
library(tigris)
library(RColorBrewer)
library(plotly)
library(GISTools)

############ LOAD DATA ######################
#Source: https://hifld-geoplatform.opendata.arcgis.com/search?groupIds=c779ef9b8468494fa2dbf7f573d61547

# Stare shape file
state_new <- readOGR("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json")

# Load state file w/ state names and abbres 
state_abbrev <- read.csv("states.csv")

# Rename Shapefile column to join on ( both are nameed state)
colnames(state_new@data)[colnames(state_new@data)=="STATE"] <- "FIPS"
colnames(state_new@data)[colnames(state_new@data)== "NAME"] <- "STATE_NAME"

# Add State Abbrevs to spatial file 
state_new <- merge(state_new, state_abbrev, by = "STATE_NAME" )

state_list <- sort(jsonlite::fromJSON("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=STATE&returnGeometry=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=true&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token=")$features$attributes$STATE)

ups_type <- sort(jsonlite::fromJSON("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=NAME&returnGeometry=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=true&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token=")$features$attributes$NAME)

header <- dashboardHeader(title = "UPS Shipping Locations", 
                          titleWidth = 450)

sidebar <- dashboardSidebar(
        sidebarMenu(id = "tabs", 
            # Menu Items ----------------------------------------------
            menuItem("Map", icon = icon("map"), tabName = "map"),
            menuItem("Table", icon = icon("table"), tabName = "table"),
            menuItem("Plots", icon = icon("bar-chart"), tabName = "plot")
            ), 
        
        ################### INPUTS ######################
        
        # Select State---------------------------------------
        
        pickerInput( inputId = "stateSelect",
                    label =  "Select State:",
                    choices = state_list,
                    options = list(`actions-box` = TRUE),
                    multiple = T, 
                    selected = c("MA")),
      
        # select Type of UPS Location--------------------------
        
        pickerInput(inputId = "typeSelect", 
                    label = "Select Type of UPS Service Location:", 
                    choices = ups_type,
                    options = list(`actions-box` = TRUE),
                    multiple = T, 
                    selected =  c("UPS DROP BOX")),

 
        # Select Input: Change Map from Circles to polygon
        radioButtons(inputId = "mapType",
                     label = "Select Map Input:",
                     choices = c("circles" = "circles", "polygons" = "polygons" ),
                     selected = c("circles")),
        
        # Download Button for Data ------------------------------- 

        downloadButton("downloadData","Download Selected Data"))
        

body <- dashboardBody(
              tabItems(
              # Data Table Page -----------------------------------
                  tabItem("table", width =12, height = 500,
                    box(title = "Data Table:", 
                    DT::dataTableOutput(outputId = "datatable"), width = 12)
                                ),
              # Leaflet Map 
                  tabItem("map",
                          title = "Map of UPS Locations",
                          leafletOutput("mapPlot")
                      ),
              # PLot Page
              tabItem("plot",
                      fluidPage(12,
                        box(title = "Histogram: Plot 1",
                          plotlyOutput("histo"))),
                      fluidPage(12,
                        box(title = "Dot Plot: Plot 2",
                        plotlyOutput("dotplot"))))
              
              ))


ui <- dashboardPage( header, sidebar, body)

server <- function(input, output) {
  
  
  ############ Get Data API CALL BY STATE (inputs in link) #####################################
  
awesome_reactive_data <-  reactive({ 
  states <- paste(input$stateSelect, collapse ="%27,%20%27")
  
  df <- readOGR(paste0("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=STATE%20IN%20(%27", states, "%27)&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=NAME%2C+STATE%2C%2C+BUSINESS_NAME%2C+CITY%2C+CENSUS_CODE%2C+PHONE%2C+LATITUDE%2C+LONGITUDE&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=standard&f=pgeojson&token="))
  df <- data.frame(df)
  df
})

  # Filter Data by state and Type of Serivce-----------------------
  state_serviceSubset <- reactive({
    req(input$stateSelect, input$typeSelect)
    data_subset <- awesome_reactive_data()[awesome_reactive_data()$STATE %in% input$stateSelect &
                                             (awesome_reactive_data()$NAME %in% input$typeSelect),]
    data_subset

  })
  
  # Filter State Shape File By State-------------------------------
  state_shape <- reactive({
    req(input$stateSelect)
    data_subset <- filter(state_new, STATE %in% input$stateSelect)
    return(data_subset)
  })
  
  # strip census_code in api call in order to merge state data on fips code to API data-------------
  state_serviceSubset_new <- reactive({
    
    new_df <- cbind(state_serviceSubset(), FIPS = substr(ups$CENSUS_CODE, 1, 2))
    return(new_df)
  })
  
  state_data_new <- reactive({
    #Merge shapefile with my dataset
    state_merge <- merge(state_new, state_serviceSubset_new(), by = c("FIPS"), sort = FALSE)
    return(state_merge)
  })
  
  # Count points in a state
  counts_by_state <- reactive({
    state_serviceSubset_new() %>% 
      group_by(STATE) %>% 
      summarise(NUM = n())
  })

  ########################### MAP #######################################
  
  # # Create Base Map
  # 
  output$mapPlot <- renderLeaflet({
    leaflet(data = state_serviceSubset())%>%
      addTiles()%>%
      addProviderTiles("Stamen.Toner", group = "Toner")
      })
      # 
  #Add Circle Layers ----------------------------------------------------------
  observe({

    if(input$mapType == "circles"){

    pal <- colorFactor(palette = "Paired", domain = c(levels(state_serviceSubset_new()$NAME)))
    
    # counts <- poly.counts(pts = state_serviceSubset_new(), polys =  state_new)
    # 
    # # # Create palette
    # pal_count <-colorNumeric(palette = "Blues", domain =counts())

    
    # pal_count <-colorNumeric(palette = "Blues", domain = counts_by_state())

    leafletProxy("mapPlot", data = state_serviceSubset_new()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      # removeMarker("markers") %>%
      clearControls() %>%
      addCircleMarkers(
                       lat = state_serviceSubset_new()$LATITUDE, 
                       lng = state_serviceSubset_new()$LONGITUDE, 
                       stroke = F, 
                       color = ~pal(NAME), 
                       group = "markers")%>%
        addLegend("bottomright", pal = pal, values = ~NAME,
                  title = "Type of UPS Location", layerId = "legend")%>%
      fitBounds(~min(state_serviceSubset_new()$LONGITUDE), ~min(state_serviceSubset_new()$LATITUDE),
                ~max(state_serviceSubset_new()$LONGITUDE), ~max(state_serviceSubset_new()$LATITUDE))
    }
    else(leafletProxy("mapPlot", data = state_data_new()) %>%
           
         #   # removeMarker("markers") %>%
           clearMarkers() %>%
           clearShapes() %>%
           clearControls() %>%
           addPolygons()
    )
    }) 
  # # Add Polygons-------------------------------------------------------------
  #   observe({
  #     if(input$mapType == "polygons"){
  #   # Create palette
  #   # pal_count <-colorNumeric(palette = "Blues", domain =counts())
  #     
  #   leafletProxy("mapPlot") %>%
  #       clearShapes()%>%
  #       removeControl()
  #       #   addPolygons(color = ~pal_count(counts_by_state()),
  #       #               weight = 2,
  #       #               opacity = 1,
  #       #               fillOpacity = 1,
  #       #               group = "UPS locations",
  #       #               highlightOptions = highlightOptions(color = "black", bringToFront = TRUE))
  #     }
  #     else(leafletProxy("mapPlot")%>%
  #                 clearShapes()%>%
  #                 removeControl("legend"))
  #     })

  # # Add Polygons -----------------------------------------------------------------
  #
  # #Source https://cran.r-project.org/web/packages/GISTools/GISTools.pdf
  #



  #  # MAP: NEED PROXY
  # output$mapPlot <- renderLeaflet({
  # 
  # 
  # # counts <- poly.counts(pts = state_serviceSubset(), polys =  state_data_new())
  # # 
  # # Create palette
  # pal_count <-colorNumeric(palette = "Blues", domain =counts())
  # 
  # 
  # leaflet(state_data_new())%>%
  #   addTiles()%>%
  #   addProviderTiles("Stamen.Toner", group = "Toner")%>%
  #   addPolygons(color = ~pal_count(counts()),
  #               weight = 2,
  #               opacity = 1,
  #               fillOpacity = 1,
  #               group = "UPS locations",
  #               highlightOptions = highlightOptions(color = "black", bringToFront = TRUE))
  # })
  
  ########################### DATA TABLE ################################ 
  
  # Data table ----------------------------------------------
  output$datatable <- DT::renderDataTable({
    
    
    DT::datatable(data = state_serviceSubset())
  })
  
############################ PLOTS ######################################
  # Histogram
  
  # Source: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Treemap
  output$histo <- renderPlotly({
    
    g <- ggplot(state_serviceSubset(), aes(STATE)) + 
      geom_bar(aes(fill=NAME), width = 0.5) + 
      theme(axis.text.x = element_text(angle=85, vjust=0.6)) + 
      labs(title="UPS Locations", 
           subtitle="Type of Shipping Location by State", x = "State") +
      theme(legend.title = element_blank()) + scale_fill_brewer(palette = "Paired")
    
    ggplotly(g)
    
  })
  # Dotplot 
  output$dotplot <- renderPlotly({
    g <-ggplot(data=state_serviceSubset(), aes(x=NAME)) + geom_point(aes(), stat="count") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(title="Total Type of Shipping Location", 
           subtitle="Sample Size varies based on selections", x = "Type of Location") +
      coord_flip() + scale_fill_brewer(palette = "Paired")
    
    ggplotly(g)
  })
  
  ############# Download Data #######################
  
  # #Output for download-----------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("UPS location data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(state_serviceSubset(), file)
    }
  )
}

shinyApp(ui, server)