library(cartogram)
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(tmap)
library(dplyr)
library(ggplot2)

data("World")

wld <- st_transform(World,crs = 3395)
wld <- filter(wld, wld$continent != "Antarctica")

count_2013 <- read.csv("C:/Users/liang/Downloads/supplychain_data/countall_bycountry_2013.csv",header = TRUE)
names(count_2013)[names(count_2013) == "country"] <- "name"

wld_new <- merge(wld, count_2013, by = "name", all.x = TRUE )

wld_new$X2013_count[is.na(wld_new$X2013_count)] <- 1

#wld_new$X2013_count_std <- (wld_new$X2013_count - mean(wld_new$X2013_count)) / sd(wld_new$X2013_count)
#wld_only <- filter(wld_new,wld_new$X2013_count != 0)

wld_cont <- cartogram_cont(wld_new, "X2013_count", itermax = 10)

#plot
pdf("cartogram-V01.pdf")

tm_shape(wld_cont) + tm_polygons("X2013_count", style = "jenks") +
  tm_layout(frame = TRUE, legend.position = c("left", "bottom"))

ggplot(wld_cont)+
  geom_sf()

#plot(st_geometry(wld_cont))

dev.off()

#----------------------------------------------------

europe_data <- read.csv("europe_data_nonans.csv")
class(europe_data)

europe_data$geometry <- st_as_sfc(europe_data$geometry, crs=3395)
sf_data <- st_as_sf(europe_data, sf_column_name = "geometry")
class(sf_data)

sf_data_transformed <- st_transform(sf_data, 3395)
sf_data_transformed
europe_cont <- cartogram_cont(sf_data_transformed, "numberchemicals", itermax = 30)

# plot it 
tm_shape(europe_cont) + tm_polygons("riskscore", style = "jenks") +
  tm_layout(frame = TRUE, legend.position = c("left", "bottom"))

# close the pdf device
dev.off()

# --------------------------------------------------
# plot not distorted data: 
pdf("cartogram_europe-notdistorted.pdf")

# Plot the map
tm_shape(sf_data_transformed) +
  tm_polygons(style = "jenks") + # Replace 'your_column_for_color' with the column you want to use for the fill color
   tm_layout(frame = FALSE, legend.position = c("left", "bottom"))


# close the pdf device
dev.off()
