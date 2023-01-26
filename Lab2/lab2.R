#   WILD562 Lab 2                                                           ####

# Author: Jordan Heiman

# Date:2023-01-26 

# Purpose: Second lab code for WILD 562, Wildlife Habitat Modeling, with Dr. 
#          Mark Hebblewhite

################################################################################
# Setup                                                                     ####


################################################################################
#   Library / Functions / Data                                              ####

#      Library                                                              ####
## Set up the needed packages
packages <- c("ks", "plotrix", "lattice", "adehabitatHR", "maptools", "mapview", 
              "raster", "ggplot2","colorRamps", "sf", "terra", "tmap", "stars", 
              "dplyr", "here")

# Function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Run function to install packages
ipak(packages)

#      Functions                                                            ####

#      Data                                                                 ####

################################################################################

# Read ub shape files 
elc_habitat <- st_read(here("Data", "elc_habitat.shp"))
humanaccess <- st_read(here("Data", "humanacess.shp"))

# If you want to see the base plot version of the maps:
plot(elc_habitat)

# 
tmap_mode("plot") # Use "view" for interactive map
tm_shape(elc_habitat) + tm_sf()
tm_shape(humanaccess) + tm_sf()

wolfyht<-st_read(here("Data", "wolfyht.shp"))
head(wolfyht)

class(wolfyht)
crs(wolfyht, proj = TRUE) # note this is a UTM projected map system. 
str(wolfyht)
# Note that there are two fields, Easting and Northing which are the X and Y 
# coordinates in UTM zone 11.  We will use these to map it for each PackID
# base plot of wolf packs by color with legend
base::plot(wolfyht$EASTING, 
           wolfyht$NORTHING, 
           col = c("red", "blue")[wolfyht$PackID], 
           ylab = "Northing", 
           xlab = "Easting")

legend(555000,
       5742500,
       unique(wolfyht$Pack),
       col = c("blue", "red"),
       pch = 1) 

# For fun - if you want to play with using package tmap()
# Construct tmap plot for Moose Winter habitat
plot.new()
tm_shape(elc_habitat) +
  tm_sf("MOOSE_W", border.alpha = 0)

# Construct ggplot2 plot for Moose Winter Habitat
# Note - color = NA in the geom_sf() removes the border lines
elk_plot <- ggplot() + 
  geom_sf(data = elc_habitat, 
          mapping = aes(fill = as.factor(MOOSE_W)), 
          color = NA) + 
  labs(x = "Easting", y = "Northing") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

# Adjust fill colors of MOOSE_W  (note that I just selected some random colors, 
# but made "7" as blue)
elk_plot2 <- elk_plot + 
  scale_fill_manual(name = "MOOSE_W",
                    values = c("gray", "gray", "red", "orange", "yellow", "green", "darkblue"))
elk_plot2

# Construct ggplot2 plot for Moose Winter Habitat
sheep_plot <- ggplot() + 
  geom_sf(data = elc_habitat, 
          mapping = aes(fill = as.factor(SHEEP_W)), 
          color = NA) + 
  labs(x = "Easting",
       y = "Northing") + 
  theme(axis.text.y = element_text(angle = 90, 
                                   hjust = 0.5))

# Adjust fill colors of SHEEP_W  (note that I just selected some random colors, 
# but made "7" as blue)
sheep_plot2 <- sheep_plot + 
  scale_fill_manual(name = "SHEEP_W",
                    values = c("gray", "gray", "red", "orange", "yellow", "green", "darkblue"))
sheep_plot2

################################################################################

# Create a mask raster to use as a template for converting shapefile data to rasters
# Create an empty raster
mask.raster <- rast()

# Set extent (note that I customized this extent so it covered both elc_habitat and humanacess)
ext(mask.raster) <- c(xmin = 443680.6, 
                      xmax = 650430.4, 
                      ymin = 5618416, 
                      ymax = 5789236) 	

# Set the resolution to 300 m 
res(mask.raster) <- 30

# Match projection to elc_habitat shapefile
crs(mask.raster) <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Set all values of mask.raster to zero
mask.raster[] <- 0

################################################################################
## This does not take as long as the raster package and fasterize is no longer needed
deer_w <- terra::rasterize(elc_habitat, 
                           mask.raster,
                           field = "DEER_W")
moose_w <- terra::rasterize(elc_habitat,
                            mask.raster,
                            field = "MOOSE_W")
elk_w <- terra::rasterize(elc_habitat,
                          mask.raster, 
                          field = "ELK_W")
sheep_w <- terra::rasterize(elc_habitat,
                            mask.raster, 
                            field = "SHEEP_W")
goat_w <- terra::rasterize(elc_habitat, 
                           mask.raster,
                           field = "GOAT_W")
wolf_w <- terra::rasterize(elc_habitat,
                           mask.raster,
                           field = "WOLF_W")

# Alternative using Stars Package - plays well with ggplot and some other things
# deer_w <- st_as_stars(elc_habitat, name = attr(elc_habitat, "DEER_W"))
deer_w_stars <- st_rasterize(elc_habitat["DEER_W"])
moose_w_stars <- st_rasterize(elc_habitat["MOOSE_W"])
elk_w_stars <- st_rasterize(elc_habitat["ELK_W"])
sheep_w_stars <- st_rasterize(elc_habitat["SHEEP_W"])
goat_w_stars <- st_rasterize(elc_habitat["GOAT_W"])
wolf_w_stars <- st_rasterize(elc_habitat["WOLF_W"])

# Plot result
plot(wolf_w)
plot(wolf_w_stars)

# Can play interactive maps with tmap_mode view
tmap_mode("view")
tm_shape(wolf_w_stars) + tm_raster() 

################################################################################
# Write raster layers to file
# writeRaster(deer_w, here("Lab2","Output", "DEER_W.tiff"), overwrite = TRUE)

################################################################################
# Reading in elevation raster with terra
elevation2 <- rast(here("Data", "Elevation2.tif")) #resampled

# Reading in elevation raster with stars
elevation2_stars <- read_stars(here("Data", "Elevation2.tif"))

# Reading in elevation raster with terra
disthumanaccess <- rast(here("Data", "DistFromHumanAccess2.tif")) 
disthighhumanaccess <- rast(here("Data", "DistFromHighHumanAccess2.tif")) 

# Resample elevation and humanaccess to match mask.raster 
# Note - not sure how to do this with stars package yet
elevation2 <- resample(elevation2, mask.raster, method = "bilinear")
disthumanaccess2 <- resample(disthumanaccess, mask.raster, method = "bilinear")
disthighhumaccess2 <- resample(disthighhumanaccess, mask.raster, method = "bilinear")

# Using Mapview -- time this takes to run varies depending on resolution
library(mapview)
mapview(wolfyht) + deer_w + sheep_w

# Tmap method -- can be faster 
tmap_mode("view")
wolf_map <- tm_shape(wolfyht) + tm_dots()
deer_layer <- wolf_map + tm_shape(deer_w_stars) + tm_raster()
deer_layer + tm_shape(sheep_w_stars) + tm_raster()

################################################################################
#      Part 1c                                                              ####

# Creating a raster from a vector layer

# First create an empty raster
dist.raster <- rast()

# Set extent 
ext(dist.raster) <- ext(humanaccess)

# Set the resolution to **30 m** (Note that this takes a very long time with a 30 m 
# resolution-even on my machine)
# Changed to 300m for runtime
res(dist.raster) <- 30

#match projection to humanaccess shapefile
crs(dist.raster) <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#set all values of dist.raster to zero
dist.raster[] <- 0

################################################################################

human.raster<-terra::rasterize(humanaccess, dist.raster, 1)
 
# Calculate distance to human access
accessdist <-system.time(distance(human.raster))

################################################################################
# First reclassify labels on humanaccess.shp file so they are correct (note: need 
# to bring in humanaccess.shp above)
levels(as.factor(humanaccess$SUM_CLASS))
# Lots of typos
#[1] "0"         "High"      "HIGH"      "Low"       "LOW"      
#[6] "MEDIUM"    "Moderate"  "Nil"       "NIL"       "VERY HIGH"

# Convert humanaccess$SUM_CLASS to a factor
humanaccess$SUM_CLASS <- as.factor(humanaccess$SUM_CLASS)

levels(humanaccess$SUM_CLASS)[1] <- "NIL"
levels(humanaccess$SUM_CLASS)[2] <- "HIGH"
levels(humanaccess$SUM_CLASS)[3] <- "LOW"
levels(humanaccess$SUM_CLASS)[5] <- "MODERATE"
levels(humanaccess$SUM_CLASS)[6] <- "NIL"

# Create indicator variable for high or not high human access
highaccess <- humanaccess[humanaccess$SUM_CLASS == "HIGH" 
                          | humanaccess$SUM_CLASS == "VERY HIGH", ]

# Use package tmap() to create simple plots
tmap_mode("plot")
humanaccess_plot <- tm_shape(humanaccess) + tm_sf()
humanaccess_plot + tm_shape(highaccess) + tm_sf(col = "red") 

################################################################################
# Home range analysis
# Just looking at one wolf pack
rd.data <- wolfyht[wolfyht$Pack == "Red Deer", ]
x <- rd.data$EASTING
y <- rd.data$NORTHING
xy <- cbind(x, y)
class(xy)
rd <- data.frame(as.character(rd.data$NAME))
coordinates(rd) <- xy
crs(rd) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

class(rd)
# Fit 99% mpc
# cp.rd <- mcp(rd, percent=99)
# Note error that one animal does not have at least 5 locations

table(rd.data$NAME)
# Looks like 4 of the wolves do not have enough locations

# Remove these individuals with too few of locations
names(rd) <- "NAME"
rd <- rd[rd@data$NAME != "69" 
         & rd@data$NAME != "81" 
         & rd@data$NAME != "82" 
         & rd@data$NAME != "84", ]

# Remove unused NAME levels
rd@data$NAME<-factor(rd@data$NAME)

## ---- warning= FALSE---------------------------------------------------------------------
# Fit 99% mcp
# Broken into individual rather than pack homerange
cp.rd <- mcp(rd, percent = 99)
plot(rd, col = "black")
plot(cp.rd[cp.rd@data$id == "42",],
     col = "blue", 
     add = TRUE)
plot(cp.rd[cp.rd@data$id == "70",],
     col = "green", 
     add = TRUE)
plot(cp.rd[cp.rd@data$id == "60",],
     col = "red", 
     add = TRUE)
plot(rd, col = "black", add = TRUE)

# Check area in square meters for each Red Deer wolf pack
as.data.frame(cp.rd)

# Calculate area for different percents of MCP in square meters. 
mcp.area(rd, percent = seq(50, 100, by = 5))

################################################################################
# Calculate 99% KDE for Red Deer wolf pack
red.deerUD <- kernelUD(rd,
                       grid = 30,
                       extent = 0.5,
                       same4all = TRUE) # reference grid
image(red.deerUD)

# Get polygons for home ranges - default 95% of kernel
homerangeRD <- getverticeshr(red.deerUD)
as.data.frame(homerangeRD)
class(homerangeRD)
plot(homerangeRD, col = 2:4)

#Estimate UD in raster mode
red.deerud <- getvolumeUD(red.deerUD) 
red.deerud

## Set up graphical parameters for the output of getvolumeUD 
par(mar = c(0, 0, 2, 0)) #set margin
image(red.deerud[[1]]) #for first wolf only
title("Red Deer Wolf UD") 
xyzv <- as.image.SpatialGridDataFrame(red.deerud[[1]]) 
contour(xyzv, add = TRUE)

################################################################################
fud <- red.deerud[[1]] #for first wolf only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[, 1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(red.deerud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)

################################################################################
#   And again for the other wolf pack  (Bow Valley)                                     ####
# First convert the spatialpointsdataframe to spatial points object
bv.data <- wolfyht[wolfyht$Pack == "Bow valley", ]
x <- bv.data$EASTING
y <- bv.data$NORTHING
xy <- cbind(x, y)

bv <- data.frame(as.character(bv.data$NAME))
coordinates(bv) <- xy
crs(bv) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Fit 99% mcp
cp.bow <- mcp(bv, percent = 99)
plot(bv, col = "black")
plot(cp.bow[cp.bow@data$id == "63",],
     col = "blue",
     add = TRUE)
plot(cp.bow[cp.bow@data$id == "87",],
     col = "red",
     add = TRUE,)
plot(cp.bow[cp.bow@data$id == "44",],
     col = "green",
     add = TRUE)
plot(bv, col = "black", add = TRUE)


#check area for each Bow Valley wolf pack
as.data.frame(cp.bow)

#calculate area for different percents of MCP
mcp.area(bv, percent=seq(50, 100, by=5))

#calculate 99% KDE for Red Deer wolf pack
bow.valleyUD <- kernelUD(bv, grid=30, extent=0.1, same4all=TRUE) # reference grid
image(bow.valleyUD)

## ----------------------------------------------------------------------------------------
#get polygons for home ranges
homerangeBV <- getverticeshr(bow.valleyUD)
as.data.frame(homerangeBV)
class(homerangeBV)
plot(homerangeBV, col=2:4)

#Estimate UD in raster mode
bow.valleyud <- getvolumeUD(bow.valleyUD) 
bow.valleyud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(bow.valleyud[[1]])
title("Bow Valley Pack UD") 
xyzv <- as.image.SpatialGridDataFrame(bow.valleyud[[1]]) 
contour(xyzv, add=TRUE)


## ----------------------------------------------------------------------------------------
fud <- bow.valleyud[[1]]
## store the value of the volume under 95% UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(bow.valleyud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)


#   Both wolf packs together                                                ####
#first convert the spatialpointsdataframe to spatial points object
x <- wolfyht$EASTING
y <- wolfyht$NORTHING
xy <- cbind(x,y)

all <- data.frame(as.character(wolfyht$Pack))
coordinates(all) <- xy
crs(all) <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Fit 99% mcp
cp.all <- mcp(all, percent = 99)

plot(all, col = "black")
plot(cp.all[cp.all@data$id == "Bow valley",], 
     col = "blue",
     add = TRUE)
plot(cp.all[cp.all@data$id == "Red Deer",], 
     col = "green",
     add = TRUE)
plot(wolfyht, col = "black", add = TRUE)

#check area for each both wolf packs
as.data.frame(cp.all)

#calculate area for different percents of MCP
mcp.area(all, percent = seq(50, 100, by = 5))

#calculate 99% KDE for both wolf packs
allUD <- kernelUD(all, grid = 30, extent = 0.5, same4all = TRUE) # reference grid
image(allUD)


################################################################################
homerangeALL <- getverticeshr(allUD)
as.data.frame(homerangeALL)
class(homerangeALL)
plot(homerangeALL, col = 2:3)

allud <- getvolumeUD(allUD) 
allud

## Set up graphical parameters for the output of getvolumeUD 
par(mar = c(0, 0, 2, 0)) #set margin
image(allud[[1]]) #for first wolf only
title("Output of getvolumeUD") 
xyzv <- as.image.SpatialGridDataFrame(allud[[1]]) 
contour(xyzv, add = TRUE)

# Need to do separate UD for each individual or pack then put them together 
# otherwise the difference in the number of points for each individual will
# weight the UD to the individual with the most points (i.e. here the bow valley 
# pack has way more points then the red deer pack)

## ----------------------------------------------------------------------------------------
fud <- allud[[1]] #for first wolf pack only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(allud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)


################################################################################
# Subset polygons by wolf pack
red.deerPOLY<-homerangeALL[homerangeALL@data$id == "Red Deer",]
bow.valleyPOLY<-homerangeALL[homerangeALL@data$id == "Bow valley",]

# Generate 1000 points from Red Deer wolf pack KDE polygon
rd.avail<-spsample(red.deerPOLY, 1000, "random")
plot(rd.avail)

# Generate 1000 points from Bow valley wolf pack KDE polygon
bv.avail<-spsample(bow.valleyPOLY, 1000, "random")
plot(bv.avail)

plot(wolfyht$EASTING,wolfyht$NORTHING, 
     col = c("red", "blue")[wolfyht$PackID],
     ylab = "Northing",
     xlab = "Easting")
legend(555000,
       5742500,
       unique(wolfyht$Pack),
       col = c("blue", "red"),
       pch = 1)
plot(bv.avail, add = TRUE)
plot(rd.avail, add = TRUE)

################################################################################
# With terra
# First re-sampling so that extents match
disthumanaccess2 <- resample(disthumanaccess, deer_w)
disthighhumanaccess <- resample(disthighhumanaccess, deer_w)

crs(elevation2) <- "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
crs(disthumanaccess2) <- "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
crs(disthighhumanaccess) <- "+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#stacking rasters
all_rasters <- c(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w, elevation2,
                 disthumanaccess2, disthighhumanaccess)

plot(all_rasters)

#with stars
#all_rasters_stars<-c(deer_w_stars, moose_w_stars, elk_w_stars, sheep_w_stars, goat_w_stars, wolf_w_stars,elevation2_stars, disthumanaccess2_stars, disthighhumanacess_stars)

class(all_rasters)

# Extract covariate values for Red Deer wolf data  
cov.outRD <- extract(all_rasters, rd.data)
head(cov.outRD)

# Extract covariate values for available points
rd.avail_sf <- st_as_sf(rd.avail) # First change rd.avail to data type sf
cov.availRD <- extract(all_rasters, rd.avail_sf)

# Extract covariate values for Bow valley wolf data  
cov.outBV <- extract(all_rasters, bv.data)

#Extract covariate values for available points
bv.avail_sf <- st_as_sf(bv.avail)
cov.availBV<-extract(all_rasters, bv.avail_sf)


## ----------------------------------------------------------------------------------------
rdused <- cov.outRD
rdused$pack <- c("Red Deer")

## repeat for Bow Valley pack
bvused <- cov.outBV
bvused$pack <- c("Bow Valley")

wolfused <- merge(rdused, 
                  bvused,
                  all.x = TRUE,
                  all.y = TRUE)
str(wolfused)
head(wolfused)

## and for next week, lets add a new column for a 1 = used 0 = avail
wolfused$used <- 1


## ----------------------------------------------------------------------------------------
summary(wolfused)


## ----------------------------------------------------------------------------------------
plot(disthumanaccess2)
plot(wolfyht, add = TRUE)


## ----------------------------------------------------------------------------------------
# Note, this replaces just NAs for just the wolf used locations with NA for 
# distance to human access, but note there were 13 missing values, say, for some 
# of the ELC H.S.I models. 
wolfused <- na.omit(wolfused)
summary(wolfused)
dim(wolfused)


## ----------------------------------------------------------------------------------------
par(mfrow = c(2,3))
hist(wolfused$DEER_W)
hist(wolfused$ELK_W)
hist(wolfused$MOOSE_W)
hist(wolfused$SHEEP_W)
hist(wolfused$GOAT_W)


## ----------------------------------------------------------------------------------------
par(mfrow = c(3,1))
hist(wolfused$Elevation2)
hist(wolfused$DistFromHumanAccess2)
hist(wolfused$DistFromHighHumanAccess2)


## ----------------------------------------------------------------------------------------
par(mfrow = c(1,1))
# Plot Bow Valley
hist(wolfused$Elevation2[wolfused$pack=="Bow Valley"],
     breaks = 50, 
     xlim = c(1400,2250),
     probability = TRUE, 
     main = "Wolf Habitat Selection", 
     xlab = "Elevation") 

# Plot Red Deer
hist(wolfused$Elevation2[wolfused$pack == "Red Deer"],
     breaks = 50, 
     col = "darkgray",
     probability = TRUE,
     add = TRUE)

# Add legend
legend("topright", 
       c("Bow Valley", "Red Deer"), 
       fill = c("white","darkgray"),
       border = "black")


## ----------------------------------------------------------------------------------------
par(mfrow = c(2,1))
multhist(list(wolfused$Elevation2[wolfused$pack=="Bow Valley"],wolfused$Elevation2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Elevation")
# I chose to put a legend in the lower right hand graph. 
# That's what the additional arguments in the line below specify.
multhist(list(wolfused$DistFromHumanAccess2[wolfused$pack=="Bow Valley"],wolfused$DistFromHumanAccess2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Distance From Humans", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))


## ----------------------------------------------------------------------------------------
par(mfrow = c(2,3))
multhist(list(wolfused$ELK_W[wolfused$pack=="Bow Valley"],wolfused$ELK_W[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Elk HSI", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$DEER_W[wolfused$pack=="Bow Valley"],wolfused$DEER_W[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Deer HSI", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$MOOSE_W[wolfused$pack=="Bow Valley"],wolfused$MOOSE_W[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Moose HSI", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$SHEEP_W[wolfused$pack=="Bow Valley"],wolfused$SHEEP_W[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Sheep HSI", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$GOAT_W[wolfused$pack=="Bow Valley"],wolfused$GOAT_W[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Goat HSI", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))


## ----------------------------------------------------------------------------------------
bwplot(ELK_W + DEER_W+MOOSE_W+ SHEEP_W+GOAT_W~pack, auto.key=TRUE,allow.multiple = TRUE,data=wolfused, outer=TRUE)

bwplot(DistFromHumanAccess2 + DistFromHighHumanAccess2 + Elevation2~pack, auto.key=TRUE,allow.multiple = TRUE,data=wolfused, outer=TRUE)


## ----------------------------------------------------------------------------------------
aggregate(Elevation2 ~ pack, data=wolfused, FUN=mean)
aggregate(DistFromHumanAccess2 ~ pack, data=wolfused, FUN=mean)
aggregate(.~pack, data=wolfused[c("pack", "DEER_W", "ELK_W", "MOOSE_W", "SHEEP_W", "GOAT_W")], mean)

sapply(wolfused, median, na.rm=TRUE)


## ----------------------------------------------------------------------------------------
library(tidyverse)
wolf_df <- as_tibble(wolfused)

wolf_df %>% 
  group_by(pack) %>% 
  summarise(mean(Elevation2))
wolf_df %>% 
  group_by(pack) %>% 
  summarise(mean(DistFromHumanAccess2))
wolf_df %>% 
  group_by(pack) %>% 
  summarise(mean(DistFromHighHumanAccess2))

wolf_df %>% group_by(pack) %>% summarise(mean(MOOSE_W))
wolf_df %>% group_by(pack) %>% summarise(mean(ELK_W))
wolf_df %>% group_by(pack) %>% summarise(mean(SHEEP_W))
wolf_df %>% group_by(pack) %>% summarise(mean(DEER_W))
wolf_df %>% group_by(pack) %>% summarise(mean(GOAT_W))
wolf_df %>% group_by(pack) %>% summarise(mean(WOLF_W))

