library(leaflet)
library(leaflet.extras)
library(ggplot2) 
library(treemapify)
library(RColorBrewer)
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

ups_locations <- readOGR("https://opendata.arcgis.com/datasets/d5c185658ec74c009ad956a92c50c58d_0.geojson")
ups <- data.frame(ups_locations@data)
ups <- subset(ups, STATE == "NY")

leaflet(ups)%>%
  addTiles()%>%
  addProviderTiles("OpenStreetMap.BZH", group = "BZH")%>%
  addAwesomeMarkers(icon = awesomeIcons(icon ="envelope", library = "glyphicon"))

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

