library(e1071)
library("dplyr")
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(maptools)


# making compatible names
load("nyc_merged_clean.Rdata")

merge_sample = merge_sample %>%
  mutate(Borough = as.factor(Borough))

# creating additional points for our 6th area that is not actually in NY.
ocean = matrix(c(-74, 40.55, 
                 -74.1, 40.51, 
                 -73.9, 40.52,
                 -73.9, 40.54,
                 -73.89, 40.56,
                 -74.1, 40.8,
                 -74.2, 40.88,
                 -74.25, 40.7,
                 -73.75, 40.85,
                 -73.71, 40.8,
                 -73.71, 40.78,
                 -73.75, 40.575,
                 -73.8, 40.58,
                 -73.79, 40.63,
                 -73.78, 40.63,
                 -73.8, 40.63,
                 -73.77, 40.63,
                 -73.76, 40.63,
                 -73.75, 40.63,
                 -73.74, 40.63,
                 -73.73, 40.63,
                 -73.72, 40.63,
                 -73.8, 40.65,
                 -74.15, 40.66,
                 -73.97, 40.8,
                 -74, 40.8,
                 -74.22, 40.6,
                 -73.825, 40.615,
                 -73.82, 40.55,
                 -73.81, 40.55,
                 -73.80, 40.55,
                 -73.79, 40.55,
                 -73.84, 40.56,
                 -73.94, 40.88,
                 -73.92, 40.565,
                 -73.97, 40.565,
                 -73.72, 40.61,
                 -74.03, 40.68,
                 -73.78, 40.82,
                 -73.775, 40.88,
                 -73.875, 40.6,
                 -73.865, 40.61,
                 -73.855, 40.62,
                 -73.845, 40.62,
                 -73.835, 40.62,
                 -73.885, 40.62,
                 -73.895, 40.62,
                 -73.855, 40.59,
                 -73.875, 40.55,
                 -74.05, 40.57,
                 -74.03, 40.59,
                 -74.05, 40.64,
                 -73.72, 40.615,
                 -74.15, 40.52,
                 -73.72, 40.7),
               nrow = 55, ncol = 2, byrow=TRUE)

ocean = as.data.frame(ocean) %>%
  mutate(Borough = "ocean",
         Borough = as.factor(Borough))
names(ocean) = c("lon", "lat", "Borough")

merge_sample = full_join(merge_sample, ocean) %>%
  mutate(Borough = as.factor(Borough))

#plot(merge_sample$lon, merge_sample$lat)

svmfit10=svm(Borough~lat+lon, data=merge_sample, kernel="radial", cost=1500, gamma=1)
# plot(svmfit10, data = merge_sample, lat~lon)

#### raster: grids

# find raster boundaries
minlat = min(merge_sample$lat) -.01
maxlat = max(merge_sample$lat) + .01
minlon = min(merge_sample$lon) -.01
maxlon = max(merge_sample$lon) + .01
r =raster(nrows = 500, ncols= 500, xmn= minlon, xmx= maxlon, ymn=minlat , ymx=maxlat)
r[]=NA
# plot(r)
# points(merge_sample$lon,merge_sample$lat,pch=16,cex=0.1)

# get lon and lat from raster
pred_locs= data.frame(xyFromCell(r, 1:250000))
names(pred_locs) = c("lon", "lat")



r[] = predict(svmfit10, pred_locs) ## pool in raster arrays: from svm to raster cells
# plot(r)
# points(merge_sample$lon,merge_sample$lat,pch=16,cex=0.1) ## dump in points

save(r, file = "raster.Rdata")

## create Polygons
polygon = rasterToPolygons(r, dissolve = T)  # warning: slow (5 mins)!!
# dump this to geoJASON
# plot(polygon, col=1:6)  # howard data has 3 classes

## geoJASON: function


## for grading: naming all Boroughs: 1: Brooklyn, 2:Brown, 3: Manhattan
polygon@data = polygon@data %>%
  mutate(layer = c("Brooklyn", "Bronx", "Manhattan", 
                   "Ocean", "Queens", "Staten Island"))
names(polygon@data) = "Name"

source("write_geojson.R")
write_geojson(polygon, file = "boroughs.json")
