library(tidyverse)
library(readr)


# ==============================================================================
# TAMS data
# ==============================================================================

# --- 1. Define File Path ---
file_path <- "C:/Users/chen.13129/Downloads/760_10202025_10202025.vcr"

# --- 2. Define Column Widths ---
# This vector matches your 103-character document exactly
column_widths <- c(
  1,  # Field 1: Record Type (RT)
  2,  # Field 2: FIPS State Code (SFIPS)
  6,  # Field 3: Station ID (ID)
  1,  # Field 4: Direction of Travel (DIR)
  1,  # Field 5: Lane of Travel (LN)
  2,  # Field 6: Year of Data (YR)
  2,  # Field 7: Month of Year (MOY)
  2,  # Field 8: Day of Month (DOM)
  2,  # Field 9: Hour of Day (HOD)
  5,  # Field 12: Total Interval Volume (TVOL)
  5,  # Field 13: Class 1 Count 
  5,  # Field 14: Class 2 Count 
  5,  # Field 15: Class 3 Count
  5,  # Field 16: Class 4 Count
  5,  # Field 17: Class 5 Count
  5,  # Field 18: Class 6 Count 
  5,  # Field 19: Class 7 Count 
  5,  # Field 20: Class 8 Count 
  5,  # Field 21: Class 9 Count
  5,  # Field 22: Class 10 Count 
  5,  # Field 23: Class 11 Count 
  5,  # Field 24: Class 12 Count 
  5,  # Field 25: Class 13 Count 
  5,  # Field 26: Class 14 Count 
  5   # Field 27: Class 15 Count 
)

# --- 3. Define Column Names ---
column_names <- c(
  "Record_Type", "State_FIPS", "Station_ID", "Direction", "Lane", 
  "Year", "Month", "Day", "Hour",
  "Total_Volume",
  "FHWA1", "FHWA2", "FHWA3", "FHWA4", "FHWA5", "FHWA6", "FHWA7",
  "FHWA8", "FHWA9", "FHWA10", "FHWA11", "FHWA12", "FHWA13", "FHWA14", "FHWA15"
)

# --- 4. Read the File ---
# This will now correctly parse all 27 fields
vcr_data <- read.fwf(
  file = file_path,
  widths = column_widths,
  header = FALSE,
  col.names = column_names,
  comment.char = "" # Prevents errors if any data has a '#'
)


head(vcr_data)





################################################################################
## Checking the `Census V-Class Hour` data and `Census Trucks Hour` data
## Both of them seems to contain Vehicle Classification data we want
## But by inspection, the `Census Trucks Hour` data is unreliable
## See below.
################################################################################

col_names_ct <- c("time", "station_ID", "substation_ID", "freeway_ID", 
                  "freeway_direction", "city_ID", "county_ID", "district_ID",
                  "abs_postmile", "station_type", "station_set_ID", "lane", 
                  "vehicle_class", "vehicle_count", "avg_speed", "violation_count",
                  "violation_code", "single_axle_count", "single_axle_count_duplicate",
                  "tandem_axle_count", "tridem_axle_count", "quad_axle_count",
                  "avg_gross_weight", "gross_weight_distribution", "avg_single_weight",
                  "avg_tandem_weight", "avg_tridem_weight", "avg_quad_weight",
                  "avg_length", "length_distribution", "avg_tandem_spacing",
                  "avg_tridem_spacing", "avg_quad_spacing",
                  "avg_wheelbase", "wheelbase_distribution", "total_flex_esal_300",
                  "total_flex_esal_285", "total_rigid_esal_300", "total_rigid_esal_285"
) 

col_names_vc <- c("time", "station_ID", "substation_ID", "freeway_ID", 
                  "freeway_direction", "city_ID", "county_ID", "district_ID",
                  "abs_postmile", "station_type", "station_set_ID", "total_flow",
                  "samples","class_1", "class_2", "class_3", "class_4", "class_5",
                  "class_6", "class_7", "class_8", "class_9", "class_10", "class_11",
                  "class_12", "class_13", "class_14", "class_15") 


file_path_truck_01012010 <- here::here("data-raw", "testing", "all_text_tmg_trucks_hour_2010_01_01.txt")
file_path_vc_01012010 <- here::here("data-raw", "testing", "all_text_tmg_vclass_hour_2010_01_01.txt")
file_path_vc_01022010 <- here::here("data-raw", "testing", "all_text_tmg_vclass_hour_2010_01_02.txt")
file_path_vc_01032010 <- here::here("data-raw", "testing", "all_text_tmg_vclass_hour_2010_01_03.txt")

#  Read the file the Jan 01, 2010 data to check
truck_data_01012010 <- read_csv(file_path_truck_01012010, col_names = col_names_ct)
vc_data_01012010 <- read_csv(file_path_vc_01012010, col_names = col_names_vc)
vc_data_01022010 <- read_csv(file_path_vc_01022010, col_names = col_names_vc)
vc_data_01032010 <- read_csv(file_path_vc_01032010, col_names = col_names_vc)

truck_data_01012010 <- truck_data_01012010 %>% 
  select(time, station_ID, substation_ID, freeway_ID, freeway_direction,
         city_ID, county_ID, district_ID, abs_postmile, station_type,
         station_set_ID, lane, vehicle_class, vehicle_count) %>%
  filter(lane == 0) # lane 0 is the sum of all lanes

vc_data_01012010 <- vc_data_01012010 %>%
  select(all_of(col_names_vc))

vc_data_01022010 <- vc_data_01022010 %>%
  select(all_of(col_names_vc))

vc_data_01032010 <- vc_data_01032010 %>%
  select(all_of(col_names_vc))

truck_data_01012010_wide <- pivot_wider(
  truck_data_01012010,
  names_from = vehicle_class,     # Makes new columns from 'vehicle_class'
  values_from = vehicle_count,    # Fills them with 'vehicle_count'
  names_prefix = "class_",        # Adds "class_" to the new column names 
)

################################################################################
## Why the census truck data is suspicious?
################################################################################

# 1. For the census truck data, the total counts do not make sense.
# class_0 is the sum of all classes
# the max is 428 vehicles per hour in a day
summary(truck_data_01012010_wide)

# The census VC data makes much more sense
summary(vc_data_01012010)
summary(vc_data_01022010)
summary(vc_data_01032010)

# 2. See station_ID = 116610 in both data
# This station is in I-5
truck_site_01012010 <- truck_data_01012010_wide %>% filter(station_ID == 116610)
vc_site_01012010 <- vc_data_01012010 %>% filter(station_ID == 116610) 
vc_site_01022010 <- vc_data_01022010 %>% filter(station_ID == 116610) 
vc_site_01032010 <- vc_data_01032010 %>% filter(station_ID == 116610) 

# The census VC data matches the PEMS system: search '116610' in Inventory
# Verify we have the same data here

# extract time 0 to 1 on 01/01/2010, 01/02/2010, 01/03/2010
test1 <- vc_site_01012010[1:2, 14:28]
test2 <- vc_site_01022010[1:2, 14:28]
test3 <- vc_site_01032010[1:2, 14:28]

# sum the north bound and south bound substations
test1 <- colSums(test1)
test2 <- colSums(test2)
test3 <- colSums(test3)

test <- bind_rows(test1, test2, test3)
summary(test)

# We have to do this since in the PEMS dashboard, we need to select at least 3
# days, and it only shows the min, max, and averages across days
# This also gives us the deinfition of Vehicle Classification codes:
# class_1 is 0-8 ft
# class_2 is 8-20 ft
# class_3 is 2 Axle, 4T SU
# class_4 is Bus
# class_5 is 2 Axle,6T SU
# class_6 is 3 Axle SU
# class_7 is 4+ Axle SU
# class_8 is < 4 Axle ST
# class_9 is 5 Axle ST
# class_10 is 6+ Axle ST
# class_11 is < 5 Axle MT
# class_12 is 6 Axle MT
# class_13 is 7+ Axle MT
# class_14 is User-Def
# class_15 is Unknown

################################################################################
# try to locate the monitors
library(sf)

# Load LRS shapefile (downloaded from Caltrans GIS portal)
lrs <- st_read(here::here("data-raw", "State_Highway_Network_Lines", "State_Highway_Network_Lines.shp"))

route <- 5
target_apm <- 42.570
direction <- c("NB")

# 1. Find segment
seg <- lrs %>%
  filter(RouteS == route,
         bOdometer <= target_apm,
         eOdometer >= target_apm,
         Direction == direction)

geom <- st_geometry(seg) %>%
  st_line_merge() %>%
  st_cast("LINESTRING")


frac <- (target_apm - seg$bOdometer) / (seg$eOdometer - seg$bOdometer)
frac

pt <- st_line_sample(geom, sample = frac) %>%
  st_cast("POINT")

pt <- st_transform(pt, 4326)

coords <- st_coordinates(pt)
coords

# can verify the accuracy at https://svctenvims.dot.ca.gov/pm_tools/

# =============================================================================
# Data we want from Georgia DOT

# install.packages("readr")
library(readr)

# 2. Define the path to your metadata file
meta_file_path <- "C:/Users/chen.13129/OneDrive - The Ohio State University/Documents/RAwork/traffic_air_pollution/data-raw/traffic_census/d03_text_meta_2010_12_11.txt"

# 3. Read the file
#    read_tsv() is specifically for tab-separated files.
station_meta <- read_tsv(meta_file_path)

# 4. View the first few rows
print(station_meta)









################################################################################
################################################################################

traffic_data <- function(state, start_year, end_year) {
  ##############################################################################
  ## This function gives clean traffic count data by vehicle class
  ##############################################################################
  ## Function inputs:
  ## state: character vector of `CA`
  ## start_year, end_year: numeric
  
  
}








