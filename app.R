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

############ LOAD DATA ######################
#Source: https://hifld-geoplatform.opendata.arcgis.com/search?groupIds=c779ef9b8468494fa2dbf7f573d61547

ups_locations <- readOGR("https://opendata.arcgis.com/datasets/d5c185658ec74c009ad956a92c50c58d_0.geojson")
# dhl_locations <- readOGR("https://opendata.arcgis.com/datasets/01e20444878040278d4d99d0bbe95654_0.geojson")
# fedex_locations <- readOGR("https://opendata.arcgis.com/datasets/13df698324c24807bc68ba7ac4f433cd_0.geojson")

ups <- data.frame(ups_locations@data)


header <- dashboardHeader(title = "UPS Shipping Locations", 
                          titleWidth = 450)

sidebar <- dashboardSidebar(
        sidebarMenu(id = "tabs", 
            # Menu Items ----------------------------------------------
            menuItem("Map", icon = icon("map"), tabName = "map"),
            menuItem("Table", icon = icon("table"), tabName = "table")), 
        
        ################### INPUTS ######################
        
        pickerInput( inputId = "stateSelect",
                    label =  "Select State:",
                    choices = sort(levels(ups$STATE)),
                    options = list(`actions-box` = TRUE),
                    multiple = T, 
                    selected = c("ME", "NH", "MA"))
)

body <- dashboardBody(theme = shinytheme("flatly"),
              tabItems(
              # Data Table Page -----------------------------------
                  tabItem("table",
                  box(title = "Data Table:", 
                  DT::dataTableOutput(outputId = "datatable"))
                                )
                        )
              )


ui <-dashboardPage( header, sidebar, body)

server <- function(input, output) {
  
  ########################### DATA TABLE ################################ 
  
  # Data table ----------------------------------------------
  output$datatable <- DT::renderDataTable({
    
    
    DT::datatable(data = ups)
  })
  

}

shinyApp(ui, server)