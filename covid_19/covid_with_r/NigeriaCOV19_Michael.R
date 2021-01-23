# A case analysis on the COVID-19 situation in Nigeria
# A geographical case analysis plotting regions with respective cases
# Data gotten from https://covid19.ncdc.gov.ng and scrapped
#********** @MICHAEL MABINUOLA ************//

# loading the neccessary libraries
install.packages("plyr")
library(rvest)
library(dplyr)
library(plyr)
library(tidyverse)
library(sf)
library(sp)# Allows us to plot the spatial polygon that the GADM data is stored as.
library(raster)  # Allows to read spatial file into
library(ggmap)
library(RColorBrewer)

# setting the working directory
setwd('/Users/kingmichael/Desktop/covid_19/covid_with_r')
getwd()

# specifying the url to get the data and make a df
url <- 'https://covid19.ncdc.gov.ng'

#Reading the HTML code from the website
webpage <- read_html(url)

# Getting all the tables
tables <- webpage %>% html_table(fill=TRUE) #FILL= TRUE to update missing vals
tables

# convert the chr columns to numeric
column_names <- c('NAME_1','Cases_Confirmed', 'Active_Cases','Discharged',
            'Total_Deaths')

# converting table to dataframe
tables_df <- as.data.frame(tables)
tables_df

#renaming the columns into the new column names.
names(tables_df) = column_names
names(tables_df)

# new_data frame
tables_df

# DATA CLEANING
# converting the chars to numeric
# clean the data by removing the commas
tables_df$Cases_Confirmed <- as.numeric(gsub(",","",tables_df$Cases_Confirmed))
tables_df$Active_Cases<- as.numeric(gsub(",","",tables_df$Active_Cases))
tables_df$Discharged <- as.numeric(gsub(",","",tables_df$Discharged))
tables_df$Total_Deaths<- as.numeric(gsub(",","",tables_df$Total_Deaths))

# change a row name so as to get the right place in Nigeria
tables_df$NAME_1[tables_df$States_Affected == 'Plateau'] <- 'Jos'
tables_df

# check the name of the dtypes
sapply(tables_df, class)

# Saving the newly created df into a CSV file formatted document
write.csv(tables_df, '/Users/kingmichael/Desktop/DA_hW/final_exam_Michael/Nigeria_Data.csv',
          row.names = FALSE)

# Reading the CSV file
nigeriaCOVID19 <- read_csv('Nigeria_Data.csv')

# statistical analysis
summary(nigeriaCOVID19)
#########################
#########################
#########################

################ GADM MAP PLOTTING ANALYSIS #############

NIGgadm <- getData("GADM", country="Nigeria", level=1) #get the level data 
colors <- rainbow(100)
plot(NIGgadm, col=colors, borders='black')
head(NIGgadm)
# all the columns
NIGgadm@data

# Join the nigeriaCOVID19 into NIGgadm based on NAME_1 column variable
NIGgadm@data <- join(NIGgadm@data, nigeriaCOVID19, by="NAME_1")
head(NIGgadm@data)

#Color code to plot the map 
RColorBrewer::brewer.pal.info
RColorBrewer::display.brewer.all()
brewer.pal.info 

# Functions to plot out tha names of each state in the country
sp.label <- function(x, label) {
  list("sp.text", coordinates(x), label)
}

NAME.sp.label <- function(x) {
  sp.label(x, x$NAME_1)
}

draw.sp.label <- function(x) {
  do.call("list", NAME.sp.label(x))
}

#Showing the total number of confirmed cases patients

#PLOTTING THE GRAPH
spplot(NIGgadm,"Cases_Confirmed", main= "Total Confirmed Cases 2020",
       col.regions = colorRampPalette(brewer.pal(9,"YlGn"))(100),
       sp.layout = draw.sp.label(NIGgadm))
dev.off()

print('From the map it is observerd that Lagos has the highest confirmed case')
print('The region is shaded dark green with a little above 25000')
print('The Federal Capital territory shaded White comes second')

#plotting total active cases

#PLOTTING THE GRAPH
spplot(NIGgadm,"Active_Cases", main= "Total Active Cases 2020",
       col.regions = colorRampPalette(brewer.pal(10,"YlOrBr"))(100),
       sp.layout = draw.sp.label(NIGgadm))
dev.off()

#plotting the total number of Deaths

#PLOTTING THE GRAPH
spplot(NIGgadm,"Total_Deaths", main= "Total Deaths 2020",
       col.regions = colorRampPalette(brewer.pal(10,"Reds"))(100),
       sp.layout = draw.sp.label(NIGgadm))
dev.off()


################## **ANOTHER WAY OF CREATING MAP WITH API ******************

# GEOPLOTTING ANALYSIS
#installing ggmap
# create an API key. 
ggmap::register_google(key='#add an api google key here')

# geocoding to get the long and lat of the cities
locations_df = tables_df['NAME_1']
locations_df
locations = mutate_geocode(locations_df, NAME_1)
locations
#creating a new dataFrame column
tables_df$lon <- locations$lon
tables_df$lat <- locations$lat
tables_df
# installing mapview package
#install.packages("mapview")
library(mapview)

# the crs represent the EPSG 4326
locations <- st_as_sf(tables_df, coords = c("lon", "lat"), crs = 4326)
# showing an animated location of all the states affected in Nigeria
mapview(locations)




















