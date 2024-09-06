library(sf)
library(tidyverse)
library(cartogram)
library(ggforce)

#READ world countries Shapefile, projection, filtering out Antarctica
world <- read_sf(dsn = "C:/Users/liang/Downloads/123.shp")
#wld <- st_transform(world,crs = 3395)
wld <- st_transform(world, crs="ESRI:54030")
wld <- filter(wld, wld$continent != "Antarctica")

#Load in-degree datasets
in2013 <- read.csv("in-2013.csv",header = TRUE)
in2015 <- read.csv("in-2015.csv",header = TRUE)
in2018 <- read.csv("in-2018.csv",header = TRUE)
in2023 <- read.csv("in-2023.csv",header = TRUE)

#Load out-degree datasets
out2013 <- read.csv("out-2013.csv",header = TRUE)
out2015 <- read.csv("out-2015.csv",header = TRUE)
out2018 <- read.csv("out-2018.csv",header = TRUE)
out2023 <- read.csv("out-2023.csv",header = TRUE)

#Change column names of iso
names(wld)[names(wld) == "iso3"] <- "ISO.a3"

#merge data to world dataset
##2013
wld_new <- merge(wld, in2013, by = "ISO.a3", all.x = TRUE)
wld_new$X2013_in_count[is.na(wld_new$X2013_in_count)] <- 0
wld_new <- merge(wld_new, out2013, by = "ISO.a3", all.x = TRUE)
wld_new$X2013_out_count[is.na(wld_new$X2013_out_count)] <- 0
##2015
wld_new <- merge(wld_new, in2015, by = "ISO.a3", all.x = TRUE)
wld_new$X2015_in_count[is.na(wld_new$X2015_in_count)] <- 0
wld_new <- merge(wld_new, out2015, by = "ISO.a3", all.x = TRUE)
wld_new$X2015_out_count[is.na(wld_new$X2015_out_count)] <- 0
##2018
wld_new <- merge(wld_new, in2018, by = "ISO.a3", all.x = TRUE)
wld_new$X2018_in_count[is.na(wld_new$X2018_in_count)] <- 0
wld_new <- merge(wld_new, out2018, by = "ISO.a3", all.x = TRUE)
wld_new$X2018_out_count[is.na(wld_new$X2018_out_count)] <- 0
##2023
wld_new <- merge(wld_new, in2023, by = "ISO.a3", all.x = TRUE)
wld_new$X2023_in_count[is.na(wld_new$X2023_in_count)] <- 0
wld_new <- merge(wld_new, out2023, by = "ISO.a3", all.x = TRUE)
wld_new$X2023_out_count[is.na(wld_new$X2023_out_count)] <- 0


#Try the map if works
ggplot(wld_new, aes(fill=X2013_in_count))+
  geom_sf()

# Set colors
col_world <- "#d3d3d3"
col_back <- "#FFFFFF"
col_2013_in <- "#fabeff"
col_2015_in <- "#f68cff"
col_2018_in <- "#ef5afc"
col_2023_in <- "#e800fc"
col_2013_out <- "#b0e4ff"
col_2015_out <- "#81d3ff"
col_2018_out <- "#47befe"
col_2023_out <- "#00a2f9"

# Set theme
theme_custom <- theme_void()+
  theme(plot.background = element_rect(fill=col_back,color=NA))

#Making Indegree Dorling cartograms------------------------------
##2013
dorl_2013_in<-cartogram::cartogram_dorling(
  wld_new, weight='X2013_in_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2015
dorl_2015_in<-cartogram::cartogram_dorling(
  wld_new, weight='X2015_in_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2018
dorl_2018_in<-cartogram::cartogram_dorling(
  wld_new, weight='X2018_in_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2023
dorl_2023_in<-cartogram::cartogram_dorling(
  wld_new, weight='X2023_in_count', k = 5,
  m_weight = 1, itermax = 1000
)

#Making Outdegree Dorling cartograms------------------------------
##2013
dorl_2013_out<-cartogram::cartogram_dorling(
  wld_new, weight='X2013_out_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2015
dorl_2015_out<-cartogram::cartogram_dorling(
  wld_new, weight='X2015_out_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2018
dorl_2018_out<-cartogram::cartogram_dorling(
  wld_new, weight='X2018_out_count', k = 5,
  m_weight = 1, itermax = 1000
)
##2023
dorl_2023_out<-cartogram::cartogram_dorling(
  wld_new, weight='X2023_out_count', k = 5,
  m_weight = 1, itermax = 1000)

#Plot default circles----------------------------
##2013in
ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.25)
  )+
  # Dorling cartogram
  geom_sf(
    dorl_2018_in,mapping=aes(geometry=geometry),
    fill=alpha(col_2018_in,0.75),color=alpha("white",0.2)
  )+
  theme_custom

#--------------------------------2013-indegree-------------------------
dorl_2013_in<-dorl_2013_in%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2013_in)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2013_in%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2013_in2 <- tibble(dorl_2013_in,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2013_in_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2013_in2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2013_in,0.75),color=alpha("white",0.2)
  )+
#  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
#             size=1, label.size = 0.1
#  )+
  theme_custom



#--------------------------------2015-indegree-------------------------
dorl_2015_in<-dorl_2015_in%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2015_in)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2015_in%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2015_in2 <- tibble(dorl_2015_in,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2015_in_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2015_in2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2015_in,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom

#--------------------------------2018-indegree-------------------------
dorl_2018_in<-dorl_2018_in%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2018_in)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2018_in%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2018_in2 <- tibble(dorl_2018_in,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2018_in_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2018_in2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2018_in,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom

#--------------------------------2023-indegree-------------------------
dorl_2023_in<-dorl_2023_in%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2023_in)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2023_in%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2023_in2 <- tibble(dorl_2023_in,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2023_in_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2023_in2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2023_in,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom



#--------------------------------2013-outdegree-------------------------
dorl_2013_out<-dorl_2013_out%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2013_out)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2013_out%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2013_out2 <- tibble(dorl_2013_out,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2013_out_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2013_out2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2013_out,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom



#--------------------------------2015-outdegree-------------------------
dorl_2015_out<-dorl_2015_out%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2015_out)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2015_out%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2015_out2 <- tibble(dorl_2015_out,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2015_out_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2015_out2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2015_out,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom

#--------------------------------2018-indegree-------------------------
dorl_2018_out<-dorl_2018_out%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2018_out)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2018_out%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2018_out2 <- tibble(dorl_2018_out,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2018_out_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2018_out2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2018_out,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom

#--------------------------------2023-indegree-------------------------
dorl_2023_out<-dorl_2023_out%>%
  mutate(
    # Compute area
    ar=as.numeric(st_area(dorl_2023_out)),
    # Compute radius based on area
    rad=as.numeric(sqrt(ar/pi))
  )

# Extract centroids for each circle
centr <- dorl_2023_out%>%
  st_centroid()%>%
  st_coordinates()

# Combine data
dorl_2023_out2 <- tibble(dorl_2023_out,X=centr[,1],Y=centr[,2])%>%
  arrange(-X2023_out_count)  

ggplot()+
  # World basemap
  geom_sf(
    wld,mapping=aes(geometry=geometry),
    fill=col_world,color=alpha("white",0.01)
  )+
  # Draw Dorling cartogram with geom_circle()
  geom_rect(
    data = dorl_2023_out2, aes(xmin = X-rad, xmax=X+rad,ymin =Y-rad,ymax = Y+rad),
    fill=alpha(col_2023_out,0.75),color=alpha("white",0.2)
  )+
  #  geom_label(data = dorl2, mapping = aes(x=X,y=Y,label=iso3),
  #             size=1, label.size = 0.1
  #  )+
  theme_custom
