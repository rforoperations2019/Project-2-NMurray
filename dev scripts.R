library(leaflet)
library(leaflet.extras)
library(ggplot2) 
library(RColorBrewer)
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
library(plotly)
library(fontawesome)

ups_locations <- readOGR("https://opendata.arcgis.com/datasets/d5c185658ec74c009ad956a92c50c58d_0.geojson")
ups <- data.frame(ups_locations@data)
ups <- subset(ups, STATE == "NY")


pal <- colorFactor(palette = "Blues", domain = c(levels(ups$NAME)))
leaflet(ups)%>%
  addTiles()%>%
  addProviderTiles("OpenStreetMap.BZH", group = "BZH")%>%
  addAwesomeMarkers(group = ups$NAME,icon = awesomeIcons(icon ="envelope", library = "glyphicon", markerColor = pal(c(levels(ups$NAME)))))%>%
  addLayersControl(
    overlayGroups = c(levels(ups$NAME)),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend("bottomright", pal = pal, values = ~NAME,
            title = "Type of UPS Location")
  


pal <- colorFactor(palette = "Paired", domain = c(levels(ups$NAME)))
leaflet(ups)%>%
  addTiles()%>%
  addProviderTiles("Stamen.Toner", group = "Toner")%>%
  addCircleMarkers(lat = ups$LATITUDE, lng = ups$LONGITUDE, group = ups$NAME, stroke = F, color = ~pal(NAME))%>%
  addLayersControl(
    overlayGroups = c(levels(ups$NAME)),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend("bottomright", pal = pal, values = ~NAME,
            title = "Type of UPS Location")


pal <- colorFactor(palette = "Paired", domain = ups_type)

# Make Icons
icons <- awesomeIcons(icon = "envelope",iconColor = 'blue',library = 'glyphicon')

leafletProxy("mapPlot", data = state_serviceSubset())%>%
  clearGroup(group = state_serviceSubset()$NAME) %>%
  addAwesomeMarkers(lng = LONGITUDE,
                    lat = LATITUDE,
                    icon = icons, group = state_serviceSubset()$NAME, ~pal(NAME))%>%
  addLayersControl(
    overlayGroups = c(levels(state_serviceSubset()$NAME)),
    options = layersControlOptions(collapsed = FALSE))%>%
  addLegend("bottomright", pal = pal, values = ~NAME,
            title = "Type of UPS Location")



state_new <- readOGR("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json")

states <- states(cb = FALSE, resolution = "500k", year = 2019)

states@data<- merge(states@data, ups, by.x = c("STUSPS"), by.y= c("STATE"), sort = FALSE)


leaflet(ups)%>%
  addTiles()%>%
  addProviderTiles("OpenStreetMap.BZH", group = "BZH")%>%
  addPolygons()



# Histogram on a Categorical variable
g <- ggplot(ups, aes(ups$STATE))
g + geom_bar(aes(fill=ups$NAME), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="UPS Locations", 
       subtitle="Type of Shipping Location by State", x = "State") +
  theme(legend.title = element_blank()) + scale_fill_brewer(palette = "Paired")


test <- readOGR("https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/UPS_Facilities/FeatureServer/0/query?where=1%3D1&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=NAME%2C%27input$typeSelect%27+STATE%2C%27input$stateSelect%27+BUSINESS_NAME%2C+CITY%2C+CENSUS_CODE%2C+PHONE&returnGeometry=true&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=standard&f=pgeojson&token=") 

