##################################################################################
# Author: Baqir Fateh                                                            #                     
# Date: March 2022                                                               #
# Modified: November 2023                                                        #
# Description: This code creates the latitude and longitude for the              #
# Afghanistan's provinces and districts using the geo-location data from UC Davis#
# website.Then, the geo-location data will be added to the household survey data #
# that has the province and district names.                                      #
##################################################################################

remove(list = ls())
gc()

library(sf)
library(tidyverse)
library(haven)

### Reading the data files if they exist in the directory
if(file.exists(here::here("gadm41_AFG_1.shp"))&
   file.exists(here::here("gadm41_AFG_2.shp"))){
  shape.file.province <- st_read(here::here("gadm41_AFG_1.shp"))
  shape.file.district <- st_read(here::here("gadm41_AFG_2.shp"))
}else{
  ### Downloading the shape files from the web if they do not exist in the directory
  temp <- tempfile()
  download.file("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_AFG_shp.zip", temp)
  
  ### Extracting the geo-code file for the provinces
  shape.file.province <- st_read(unzip(temp,"gadm41_AFG_1.shp"))
  
  ### Extract the geo-code file for the districts
  shape.file.district <- st_read(unzip(temp,"gadm41_AFG_2.shp"))
  
}

### Processing the shape files 

## Defining the shape files
shape.files <- list(shape.file.province, shape.file.district)

# Initialize an empty list to store the final data frames
shape.file.final <- list()

# Looping through the shape files 
for (i in seq_along(shape.files)) {
  
  # Extracting centroids
  centroids <- st_centroid(shape.files[[i]])
  
  # Conditioning on the shape files
  if (i == 1) {
    # Province shape file conditions 
    shape.files[[i]]$latitude.prov <- st_coordinates(centroids)[, 2]
    shape.files[[i]]$longitude.prov <- st_coordinates(centroids)[, 1]
    variable.selection <- c("name_1", "latitude.prov", "longitude.prov")
    col.name <- c(province = "name_1")
  } else {
    # District shape file
    shape.files[[i]]$latitude.dist <- st_coordinates(centroids)[, 2]
    shape.files[[i]]$longitude.dist <- st_coordinates(centroids)[, 1]
    variable.selection <- c("name_1", "name_2", "latitude.dist", "longitude.dist")
    col.name <- c(province = "name_1", district = "name_2")
  }
  
  # Converting variable names to lowercase
  names(shape.files[[i]]) <- tolower(names(shape.files[[i]]))
  
  # Selecting relevant variables and renaming them
  final.data <- shape.files[[i]] %>%
    select(all_of(variable.selection)) %>%
    rename(!!!col.name) %>%
    as.data.frame()
  
  # Storing the final data frame in the list
  shape.file.final[[i]] <- final.data
}

## Adding the province geo-code from the province shape file to the district shape file
master.shape.file <- shape.file.final[[1]]%>%
  left_join(shape.file.final[[2]], by = "province")%>%
  select(province, district, latitude.dist, latitude.prov, longitude.dist, longitude.prov)

## Saving the shape file 
write_csv(master.shape.file, "master.shape.file.csv")

### Adding the geo-location info to the main survey data data file
## Uploading the main survey data set
srvy.data.raw <- read_dta(here::here("raw.data/weighted.data.final.dta"))

## Merging the province and district geo-location data with the survey data
srvy.data.final <- srvy.data.raw%>%
  left_join(master.shape.file, by = c("province", "district"))

## Save the final data file
write_dta(srvy.data.final, "clean_data/clean.data.final.dta")

############################## END ################################################

