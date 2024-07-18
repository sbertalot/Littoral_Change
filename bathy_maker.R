# Load required libraries
library(terra)
library(dplyr)
library(gstat)


# Read contour lines from shapefile
mapped_points <- read.csv("/Users/seanbertalot/Documents/Aqua_Sat_Littoral/Data/Bathymetric Data/FGR/Points/FGR_contours.csv")
mapped_points <- mapped_points %>% rename(x = X, y = Y, depth_ft = Depth.m.)
contour_points <- read.csv("/Users/seanbertalot/Documents/Aqua_Sat_Littoral/Data/Bathymetric Data/FGR/Contours/contour_points.csv")

mapped_points$depth_ft <- mapped_points$depth_ft * 3.281

merged_points <- rbind(mapped_points, contour_points)
shape <- vect("Data/Shapefiles/Wyoming/FGR/0_ft.shp")
shape <- project(shape, "EPSG:4326")

#A function that creates an empty grid the size of a given lakes surface
create_grid <- function(shape) {
  
  #Creates an empty raster with the extent lake shapefile
  empty.ras <- rast(shape)
  #Increases the resolution of the raster by a factor of 200
  disagg.ras <- disagg(empty.ras, fact = 200)
  #creates a raster of the shape outline in the grid dissagg.ras
  ras <- rasterize(shape, disagg.ras)
  
  #masks the rasater for the shapefile (everything outside the reservoir = NA)
  masked_raster <- mask(ras, shape)
  
  return(masked_raster)
}

#A function that generates the DEM for a given lakes contours 
DEM_maker <- function(df, grid, outline) {
  #Interpolation function for deriving contours
  gs <- gstat(formula=depth_ft~1, 
              locations=~x+y, #Locations correspond to the locations of the sampling sites
              data=df,
              nmax = 50,#nmax set to the number of rows in your data sets means that the number of neighbors is set to the total number of locations
              set = list(idp = 0))
  
  #remove NA cells from the grid (interpolate doesn't like NA's)
  grid <- na.omit(grid)
  #Create DEM's for both lakes with the interpolate function and the gstat formula above
  DEM <- terra::interpolate(grid, gs)
  
  #mask the interpolation so that you reintroduce NA's where there's no water
  mask.na <- init(grid, NA)
  #create a grid with only 0 where the land is
  mask.2 <- init(grid, 0)
  
  #replaces NA values with the max height
  replace.na <- cover(grid, mask.2, values = NA)
  #fills the lake with NA values 
  replace.water <- cover(replace.na, mask.na, values = 1)
  
  #merges the two rasters (one has the perimeter of the lake set to 0, one has the bathymetry)
  final.DEM <- merge(replace.water, DEM)
  
  return(final.DEM)
}


grid <- create_grid(shape)

DEM <- DEM_maker(merged_points, grid, shape)


