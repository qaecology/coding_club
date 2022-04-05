# Extracting environmental variables using R packages
# qaeco coding club
# 31-Mar-2022
# Alys Young



# 1. Set up ------------------------------------------------------------------
# Install if needed and then load these packages
library(dplyr) # data cleaning
library(lubridate) # dates
library(sf) # spatial objeccts like points
library(raster) # the raster gridded data


library(chirps) # rainfall from the CHIRPS satellite
library(slga)  # soil from the Soil and Landscape Grid of Australia
library(MODISTools) # MODIS satellite images





# 2. Points to extract --------------------------------------------------------------------

# make up some points
points_df <- data.frame(lon = c(142.2, 145.1), # longitude
                        lat = c(-30.1, -32.8), # latitude
                        date = as_date(c("2021-06-23", "2020-12-20")))
points <- st_as_sf(points_df,
                   coords = c("lon","lat") )

points$lon  <- st_coordinates(points)[,1]
points$lat  <- st_coordinates(points)[,2]






# 3. CHIRPS - rainfall -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CHIRPS: Rainfall Estimates from Rain Gauge and Satellite Observations
# 0.05° resolution
# daily, pentadal, and monthly rainfall estimates
# 1981-present
# The paper: https://www.nature.com/articles/sdata201566



## Create the dataframe to save into

# time periods to extract the apply the function (e.g. mean) over
# time in the length of days
days <- c(10, 30, 180) # 10 days, 1 month, 6 months

# set up a dataframe to save the results
chirps_df <- data.frame(matrix(NA,
                               nrow = nrow(points_df), # number of rows for the output is the same as the number of points
                               ncol = length(days)+ncol(points_df))) # enough columns to save the value of each of the time periods, plus all the point information
names(chirps_df) <- c(colnames(points_df), paste0("RainTot_", days, "_days"))

# have a look at the blank dataframe to save the results
chirps_df


## One point ------------------------------------------------------------------------------------------------------------------------------------

## select one point
point <- points[1,]

## set the start and end dates to extract from
start_date <- point$date - 10 # 10 days before the survey date
end_date <- point$date # date of the survey

## extract the data
dat <- get_chirps(point,
                  dates = c(as.character(start_date), as.character(end_date)),  # must be as character, not date
                  server = "ClimateSERV") # better server if doing a small amount of points and dates. Leave as the default for bigger requests
                  # # server = "CHC" - default, for multiple sites and years


## look at the data
dat

## create indices
p_ind <- precip_indices(dat, # the chirps dataframe
                        timeseries = TRUE,
                        intervals = as.numeric(end_date - start_date)) # difference between the dates, add one to the day to capture all the data

## list of the indices
# https://rdrr.io/cran/chirps/man/precip_indices.html

## look at the data
p_ind

## get the value youre interested in
# here its the total
v <- p_ind$value[p_ind$index == "Rtotal"]


## Loop ---------------------------------------------------------------------------------------------------------------------------------------
# NOTE: the year of the start and end dates

# the variables set earlier
# days are 10, 30, and  180 # OR 10 days, 1 month, 6 months


for(i in 1:nrow(points_df)){ # for each point

  # get one point at a time
  point <- points[i,]

  # same the point into to the dataframe
  # the new dataframe column names are in ''
  chirps_df[i, 'lon'] <- point$lon
  chirps_df[i, 'lat'] <- point$lat
  chirps_df[i, 'date'] <- point$date

  # for each time period
  for(ii in 1:length(days)){

    ## dates
    start_date <- point$date - days[ii] # calculate the date of the time period length before the date given
    end_date <- point$date #

    ## Ifelse statement to see if the year of the start and end are in the same year.
    #if they are, only need to extract once
    # if the years are different, need to extract the two years of data seperately

    ## If the time periods goes across 1 year
    if(year(start_date) == year(end_date)){ # year of the start and end are the same

      ## Download the CHIRPS data
      # returns a dataframe
      dat <- get_chirps(point,
                        dates = c(as.character(start_date), as.character(end_date)),  # must be as character, not date
                        server = "ClimateSERV") # better server if doing a small amount of points and dates. Leave as the default for bigger requests

      ## create indices
      p_ind <- precip_indices(dat, # the chirps dataframe
                              timeseries = TRUE,
                              intervals = days[ii] + 1) # add one to the day to capture all the data

      ## get the value youre interested in
      # here its the total
      v <- p_ind$value[p_ind$index == "Rtotal"]

    } else { ## If the years of the start and end are different. Need to exact the years seperately

      # first year
      start_date <- point$date - days[ii]
      end_date_y1 <- as.date(paste0(year(start_date),"-12-31")) # last day of the year
      # Second year
      start_date_y2 <- as.date(paste0(year(end_date),"-01-01")) # first day of the year
      end_date <- point$date

      # difference in the dates
      diff1 <- as.numeric(end_date_y1 - start_date)
      diff2 <- as.numeric(end_date - start_date_y2)

      ## year 1
      ## Extract the data
      dat1 <- get_chirps(point,
                         dates = c(as.character(start_date), as.character(end_date_y1)),
                         server = "ClimateSERV")
      ## create indices - after merging the two datasets - TO DO HERE
      p_ind1 <- precip_indices(dat1,
                               timeseries = TRUE,
                               intervals = diff1 + 1)

      ## get value
      v1 <- p_ind1$value[p_ind1$index == "Rtotal"]


      ## year 2
      ## Extract the data
      dat2 <- get_chirps(point, # returns a dataframe
                         dates = c(as.character(start_date_y2), as.character(end_date)),
                         server = "ClimateSERV")
      ## create indices
      p_ind2 <- precip_indices(dat2,
                               timeseries = TRUE,
                               intervals = diff2 + 1)

      ## get value
      v2 <- p_ind2$value[p_ind2$index == "Rtotal"]

      ## for the total of the first and second year, add together
      v <- v1 + v2

    }

    ## save the total rainfall value out
    chirps_df[i, ii + 3] <- v

    ## Check how the progress is going by printing a message
    # print(paste0("Total rainfall from ", end_date ," for ", days[ii], " days before to ",start_date,": ", v ))
  }
}

## look at the final dataframe
chirps_df












# 4. SLGA - soil  ------------------------------------------------------------------------------------------------------------------------------------------------------
# Soil and Landscape grid of Australia
# https://www.clw.csiro.au/aclep/soilandlandscapegrid/
# raster only

# # Slga info
# use these if you want to loop over multiple variables or depths
# soil_attributes_d <- c("CLY", "SOC", "SND", "SLT", "NTO", "PTO")
# soil_attributes_d_names <- c("Clay", "Organic Carbon", "Sand", "Silt", "Total Nitrogen", "Total Phosphorus")
# soil_depths <- 1:5 # total of 6, only use 5 to not include the depth of 100-200cm
# soil_depth_dist <- c("0to5cm", "5to15cm", "15to30cm", "30to60cm", "60to100cm", "100to100cm")



## One point  -----------------------------------------------------------

point <- points[1,]

## create a small area around your point to download
# this speeds up the download time
# the numbers are degrees.
aoi <- c(point$lon - 0.02, point$lat - 0.02, point$lon + 0.02, point$lat + 0.02)

ras <- get_soils_data(product = 'NAT',
                      attribute = 'CLY', # soil clay content
                      component = 'VAL', # value. not the confidence intervals
                      depth = 1, # 0 to 5 cm
                      aoi = aoi, # area of interest. small area define earlier around your site
                      write_out = FALSE)

## have a look
plot(ras)
points(point[c('lon', 'lat')] %>% st_drop_geometry()) # this is longer than it needs to be. It used to work just using points(point) but now throws me an error.

## Extract values from the raster
point.values <- raster::extract(ras, point[,c('lon', 'lat')])

## OR extract with a buffer distnace in meters
pointBuf.values <- raster::extract(ras, point[,c('lon', 'lat')], buffer = 250) # e.g. buffer of 250m
# summarise the buffer values
point.values2 <- mean(unlist(pointBuf.values))




## Loop -------------------------------------------------------------------------------------------------------------------------------------
# multiple attributes and points
# and a buffer


## The soil attributes to extract
soil_attributes_d <- c("CLY","SND", "NTO", "PTO")

## set up a dataframe
SLGA_df <- data.frame(matrix(NA,
                             nrow = nrow(points_df),
                             ncol = length(soil_attributes_d) + ncol(points_df)))
names(SLGA_df) <- c(colnames(points_df), soil_attributes_d)



## Nested loop
# for each point,
for(i in 1:nrow(points_df)){

  # get one point
  point <- points[i,]

  aoi <- c(point$lon - 0.02, point$lat - 0.02, point$lon + 0.02, point$lat + 0.02)

  SLGA_df[i, 'lon'] <- point$lon
  SLGA_df[i, 'lat'] <- point$lat
  SLGA_df[i, 'date'] <- point$date

  # for each of the soil attributes
  for(ii in 1:length(soil_attributes_d)){

    # Get the data
    ras <- get_soils_data(product = 'NAT',
                          attribute = soil_attributes_d[ii],
                          component = 'VAL', # value. not the confidence intervals
                          depth = 1, # 0 to 5 cm
                          aoi = aoi,
                          write_out = FALSE)


    ## Extract values from the raster
    point.values <- raster::extract(ras, point[,c('lon', 'lat')])

    # ## OR extract with a buffer distnace in meters
    # pointBuf.values <- try(raster::extract(ras, point[,c('lon', 'lat')], buffer = 250)) # e.g. buffer of 250m
    # # summarise the buffer values
    # point.values2 <- mean(unlist(pointBuf.values))

    ## save the data out to the dataframe using the soil attribute names for the column names
    SLGA_df[i, soil_attributes_d[ii]] <- point.values # or point.values2 for the buffer

  }
}


## look at the result
SLGA_df





# 5. MODIS for NDVI ----------------------------------------------------------
# MODIS - 
# NDVI and EVI are 250m resolution and 16 day return interval


## Look at the products
mt_products() %>%  arrange(frequency) # try arranging it by other measures like resolution_meters
# MOD13Q1     MODIS/Terra Vegetation Indices (NDVI/EVI) 16-Day L3 Global 250m SIN Grid      16 day    250
# MYD13Q1     MODIS/Aqua Vegetation Indices (NDVI/EVI) 16-Day L3 Global 250m SIN Grid       16 day    250

## look at the bands
mt_bands(product = "MOD13Q1") # MOD13Q1 for Terra MODIS ndvi



## One point ------------------------------------------------------------

## The points
point <- points[1,]
point$date <- as_date(point$date) # change to a date format

## Set up the dates to extract for
start_date_180 <- point$date - 180 # 6 months
start_date_90 <- point$date - 90 # 3 months
end_date <- point$date

# download data
subset <- mt_subset(product = "MOD13Q1",
                    lat = point$lat, # latitude
                    lon = point$lon, # longitude
                    band = "250m_16_days_NDVI", # chosen earlier using mt_products() and mt_bands()
                    start =  start_date_180,
                    end = end_date,
                    km_lr = 0, # buffer region - left and right in kms
                    km_ab = 0, # buffer region - above and below in kms
                    site_name = "testsite",
                    internal = TRUE,
                    progress = TRUE) # TRUE


## Apple a scale factor of 0.0001
subset <- subset %>% mutate(ndvi = value * 0.0001)


## Filter the 6 month dataset to the dates that you want
subset_90 <- subset %>%
  filter(calendar_date >= start_date_90)

## Apply the function to get the summarised value you want. e.g. mean
subset_90_ndvi <- mean(subset_90$ndvi)
subset_180_ndvi <- mean(subset$ndvi)


## To average over an area - e.g. the size of the samplng site
# use the km_lr and km_ab variables - distance as integer in kilometers
  # km_lr is kilometers left and right
  # km_ab is kilometers above and below
# the result returns you the same structured dataframe with new rows for each pixel in the kilometer buffer

subset_1kmbuffer <- mt_subset(product = "MOD13Q1",
                    lat = point$lat,
                    lon = point$lon,
                    band = "250m_16_days_NDVI",
                    start =  start_date_180,
                    end = end_date,
                    km_lr = 1, # 1km buffer
                    km_ab = 1, # 1km buffer
                    site_name = "testsite",
                    internal = TRUE,
                    progress = TRUE) # TRUE

## Apply a scale factor of 0.0001
subset_1kmbuffer <- subset_1kmbuffer %>% mutate(ndvi = value*0.0001)

## Filter the 6 month dataset to the dates that you want
subset1km_90 <- subset_1kmbuffer %>%
  filter(calendar_date >= start_date_90)

## Apply the function to get the summarised value you want. e.g. mean
subset1km_90_ndvi <- mean(subset1km_90$ndvi)
subset1km_180_ndvi <- mean(subset_1kmbuffer$ndvi)


## Loop --------------------------------------------------------------------
# loop through the multiple sites
# calculate for the multiple time periods (not a loop)

# The time periods
time_periods <- c(180, 90, 15)


## set up a dataframe
ndvi_df <- data.frame(matrix(NA,
                             nrow = nrow(points_df),
                             ncol = length(time_periods) + ncol(points_df)))
names(ndvi_df) <- c(colnames(points_df), paste0("ndvi_", time_periods))

ndvi_df$date <- as.Date(ndvi_df$date)




for(i in 1:nrow(var_dynamic_sitesdf5)){

  # get one point
  point <- points[i,]
  point$date <- as.Date(point$date)

  ## Fill in the results dataframe
  ndvi_df[i,'date'] <- point$date
  ndvi_df[i,'lon']  <- point$lon
  ndvi_df[i,'lat']  <- point$lat

  ## Set the dates
  start_date_180 <- point$date - time_periods[1] # 180 days
  start_date_90  <- point$date - time_periods[2] # 90 days
  start_date_15  <- point$date - time_periods[3] # 15 days
  end_date       <- point$date

  ## download the data
  subset <- mt_subset(product   = "MOD13Q1",
                      lat       = point$lat,
                      lon       = point$lon,
                      band      = "250m_16_days_NDVI",
                      start     = start_date_180, # the biggest time period. Then filter the data after
                      end       = end_date,
                      km_lr     = 0,
                      km_ab     = 0,
                      #point_name = "testpoint",
                      internal  = TRUE,
                      progress  = TRUE) # TRUE

  ## apply the scale factor
  subset <- subset %>%
    mutate(ndvi = value * 0.0001)

  ## Filter by the dates for the shorter time periods
  subset_90 <- subset %>% filter(calendar_date >= start_date_90)
  subset_15 <- subset %>% filter(calendar_date >= start_date_15)

  ## save the values out
  ndvi_df[i,'ndvi_15'] <- mean(subset_15$ndvi)
  ndvi_df[i,'ndvi_90']     <- mean(subset_90$ndvi)
  ndvi_df[i,'ndvi_180']    <- mean(subset$ndvi)

  ## check how the loop is progressing with a print statement
  print(paste0(t,"/", nrow(var_static_pointsdf5), " points done."))
}

## look at the result
ndvi_df

