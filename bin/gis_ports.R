# Jennifer Selgrath
# NOAA CINMS
#
# GOAL: create map of study sites from GIS file
# ---------------------------------------------------------------------
library(tidyverse)
library(usmap) #import the package. Albers Equal Area projection. This returns a ggplot object.
library(ggplot2) #use ggplot2 to add layer for visualization. 
library(sf)
# library(maptools)
library(colorspace)
library(ggspatial)
library(rgdal)

# ----------------------------
remove(list=ls())
setwd("C:/Users/Jennifer.Selgrath/Documents/research/R_projects/dsc_valuation/")



#read
d1<-read_csv("./data/ports_n_to_s.csv")

d.sf<-read_sf("./gis/cdfw_ports/cdfw_marine_landing_ports_R7.shp")
plot(d.sf)
names(d.sf)

unique(d.sf$PortCode)

# merge list of CDFW ports by landing data to get subset / port areas
d.sf2<-d.sf%>%
  merge(d1,by="PortCode")

# check out the data
plot(d.sf2$PortCode)

str(d.sf2) 
st_crs(d.sf2) 
d.sf2   #WGS 84 / Pseudo-Mercator
data.frame(d.sf2)
names(d.sf2)



# Transform coord to lat/long for usmaps
d.sf3<-st_transform(d.sf2, CRS("+proj=longlat +datum=WGS84"))
st_crs(d.sf3)
d.sf3 



# -----------------------------------------------------------------
# pull out coordinates from shapefile of ports
d.sf4 <- d.sf3 %>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))%>%
  glimpse()

# another option
# dplyr::mutate(lon = sf::st_coordinates(.)[,1],
#               lat = sf::st_coordinates(.)[,2])


d.sf4 
str(d.sf4)
st_crs(d.sf4)




d2<-data.frame(d.sf4)%>%glimpse()

# usmap_transform is provided that transforms a data.frame containing longitude/latitude columns to use this projection
usmap_crs()
d.sf5<-usmap_transform(d2,
                         input_names=c("long","lat"),
                         output_names=c("x","y"))
str(d.sf5)
# test
plot_usmap(include="CA") +
  # plot_usmap(include = c("CA", "AZ", "NV", "OR"), color = "lightgray")+
  geom_point(data=d.sf5,aes(x=x,y=y,color=port_order,size=5))+
  geom_point(data=d.sf5,aes(x=x,y=y),shape = 1,size = 5,colour = "black")+
  # geom_point(shape = 1,size = 5,colour = "black")+

geom_text(data=d.sf5,aes(x=x,y=y,label=PortGroup_IOPAC,color=port_order),size=4,vjust=-.4,hjust=-.1)+ 
  # scale_color_continuous_sequential(palette = "Hawaii")+
  scale_color_continuous_sequential(palette = "TealGrn")+
  labs(title = "California Port Groups") + 
  theme(panel.background = element_rect(color = "black", fill = "darkgrey"),legend.position="none")+
# maps
# usmap_crs()@projargs


# plot

  # scales
  # https://stackoverflow.com/questions/61809382/how-can-i-put-a-scalebar-and-a-north-arrow-on-the-map-ggplot
  annotation_scale(
    location = "tr",
    bar_cols = c("grey50", "white"),
    pad_x = unit(0.1, "in"), pad_y = unit(.7, "in"),
    text_col="black", text_face="bold")+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20")
  )


ggsave("./doc/cdfw_ports_map2.tiff", width=8, height=6)
