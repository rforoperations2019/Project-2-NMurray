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
  addProviderTiles(providers$Stamen.Toner)%>%
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

# states <- states(cb = FALSE, resolution = "500k", year = 2019)

# strip census code in api call in order to merge state data to API data
ups$FIPS <- substr(ups$CENSUS_CODE, 1, 2)

# Rename Shapefile column to join on ( both are nameed state)
colnames(state_new@data)[colnames(state_new@data)=="STATE"] <- "FIPS"

state_new@data<- merge(state_new@data, ups, by = c("FIPS"), sort = FALSE)

pal <- colorFactor(palette = "Paired", domain = c(levels(ups$NAME)))
leaflet(state_new)%>%
  addTiles()%>%
  addProviderTiles("Stamen.Toner", group = "Toner")%>%
  addPolygons()
  
  # addCircleMarkers(lat = ups$LATITUDE, lng = ups$LONGITUDE, group = ups$NAME, stroke = F, color = ~pal(NAME))%>%
  # addLayersControl(
  #   overlayGroups = c(levels(ups$NAME)),
  #   options = layersControlOptions(collapsed = FALSE))%>%
  # addLegend("bottomright", pal = pal, values = ~NAME,
  #           title = "Type of UPS Location")

#########################################################
#heat map

leaflet(ups)%>%
  addTiles()%>%
  addProviderTiles(providers$Stamen.Toner)%>%
  addHeatmap(lng = ups$LONGITUDE, lat = ups$LATITUDE, intensity = ups$FIPS)



###################


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


# Dot Plot 

 ggplot(data=ups, aes(x=NAME)) + geom_point(aes(), stat="count") + 
   theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
   labs(title="Total Type of Shipping Location", 
        subtitle="Sample Size varies based on selections", x = "Type of Location") +
   coord_flip() + scale_colour_brewer(palette = "Paired")
