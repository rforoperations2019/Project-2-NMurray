## app.R ##
library(shinydashboard)
library(httr) # http rest requests
library(jsonlite) # fromJSON
library(utils) # URLencode functions
library(rgdal) # readOGR
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(shinyjs)
library(rgeos)
library(shinyWidgets)
library(tigris)
library(RColorBrewer)
library(plotly)

############ LOAD DATA ######################
#Source: https://hifld-geoplatform.opendata.arcgis.com/search?groupIds=c779ef9b8468494fa2dbf7f573d61547

# ups_locations <- readOGR("https://opendata.arcgis.com/datasets/d5c185658ec74c009ad956a92c50c58d_0.geojson")
# # dhl_locations <- readOGR("https://opendata.arcgis.com/datasets/01e20444878040278d4d99d0bbe95654_0.geojson")
# # fedex_locations <- readOGR("https://opendata.arcgis.com/datasets/13df698324c24807bc68ba7ac4f433cd_0.geojson")
# 
# ups <- data.frame(ups_locations@data)
# ups <- subset(ups, STATE == c("NY"))  # Filter by Northeast

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
                    label = "Select Type of UPS Service Location", 
                    choices = ups_type,
                    options = list(`actions-box` = TRUE),
                    multiple = T, 
                    selected =  c("UPS DROP BOX")))

        # NEED ANOTHER INPUt!!!!!!!!!!!!!!!!!!!!!!!
        
        # Download Button for Data ------------------------------- #####NEED! 
        
        # downloadButton("downloadData", "Download Selected Data")
        

body <- dashboardBody(theme = shinytheme("flatly"),
              tabItems(
              # Data Table Page -----------------------------------
                  tabItem("table",
                    box(title = "Data Table:", 
                    DT::dataTableOutput(outputId = "datatable"))
                                ),
              # Leaflet Map 
                  tabItem("map",
                          title = "Nora's Map",
                          leafletOutput("mapPlot")
                      ),
              # PLot Page
              tabItem("plot",
                      box(title = "Plot 1",
                          plotlyOutput("histo")), width = 10)
              ))


ui <- dashboardPage( header, sidebar, body)

server <- function(input, output) {
  
  
  ############ Get Data API CALL BY STATE and NAME (inputs in link) #####################################
  
awesome_reactive_data <-  reactive({ 
         # df <- readOGR("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=STATE%3D%27input$stateSelect%27&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token=")
  
  # # # More fields
  # df<-readOGR("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=NAME%2C%27input$typeSelect%27+STATE%2C%27input$stateSelect%27+BUSINESS_NAME%2C+CITY%2C+CENSUS_CODE%2C+PHONE&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=standard&f=pgeojson&token=")
  
  
 df<-readOGR("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=NAME%2C%27input$typeSelect%27+STATE%2C%27input$stateSelect%27+BUSINESS_NAME%2C+CITY%2C+CENSUS_CODE%2C+PHONE%2C+LATITUDE%2C+LONGITUDE&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=standard&f=pgeojson&token=")
 

  df <- data.frame(df@data)
})
  
  # Filter Data by state and Type of Serivce
  state_serviceSubset <- reactive({
    req(input$stateSelect, input$typeSelect)
    data_subset <- awesome_reactive_data()[awesome_reactive_data()$STATE %in% input$stateSelect &
                                             (awesome_reactive_data()$NAME %in% input$typeSelect),]
    data_subset

  })
  ########################### MAP #######################################
  
  output$mapPlot <- renderLeaflet({
    leaflet(state_serviceSubset())%>%
      addTiles()%>%
      addProviderTiles("OpenStreetMap.BZH", group = "BZH")%>%
      addAwesomeMarkers(icon = awesomeIcons(icon ="envelope", library = "glyphicon"))
  })
  
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
      theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
      labs(title="UPS Locations", 
           subtitle="Type of Shipping Location by State", x = "State") +
      theme(legend.title = element_blank()) + scale_fill_brewer(palette = "Paired")
    
    ggplotly(g)
    
  })
}

shinyApp(ui, server)