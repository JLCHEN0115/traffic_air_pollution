# ==============================================================================

if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,  
  data.table, 
  sf,         
  lubridate,  
  progress,   
  here        
)

# TAMS Data Specs 
column_widths_vcr <- c(1, 2, 6, 1, 1, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
column_names_vcr <- c("Record_Type", "State_FIPS", "Station_ID", "Direction", "Lane", 
                      "Year", "Month", "Day", "Hour", "Total_Volume", 
                      "FHWA1", "FHWA2", "FHWA3", "FHWA4", "FHWA5", "FHWA6", "FHWA7",
                      "FHWA8", "FHWA9", "FHWA10", "FHWA11", "FHWA12", "FHWA13", 
                      "FHWA14", "FHWA15")

# ==============================================================================
# Helper Function to Get Point from LRS (for PeMS census data)
# Note that the postmile we have is absolute postmile (that does not reset to 0
# at county boundary). We use the information about freeway number, direction,
# absolute postmile to get the exact latitude and longitude. (See README file)
# ==============================================================================
get_point_from_lrs <- function(station_info, lrs_data) {
  # Takes a single row (as data.table) of unique station info and the LRS data
  # Returns a data.table with substation_ID and geometry (or NULL on failure)
  
  route <- station_info$freeway_ID
  target_apm <- station_info$abs_postmile # Use the absolute postmile
  direction_code <- station_info$freeway_direction
  
  # Map PeMS direction to LRS direction (adjust if needed based on LRS data values)
  direction_map <- c("N" = "NB", "S" = "SB", "E" = "EB", "W" = "WB")
  direction <- direction_map[direction_code]
  
  # Basic validation
  if (is.na(direction) || is.na(target_apm) || is.na(route) || target_apm < 0) {
    
    warning(paste("Invalid direction, route, or postmile (<0) for substation:", station_info$substation_ID, 
                   "Route:", route, "APM:", target_apm, "Dir:", direction_code))
    return(NULL)
  }
  
  # Filter LRS segments matching Route and Direction, covering the target APM
  seg <- lrs_data %>%
    filter(RouteS == as.character(route),
           Direction == direction,
           bOdometer <= target_apm,
           eOdometer >= target_apm)
  
  # Handle cases where no direct overlap is found 
  if (nrow(seg) == 0) {
      #seg <- lrs_data %>%
      #filter(RouteS == as.character(route),
      #       Direction == direction,
      #       abs(bOdometer - target_apm) < 0.001 | abs(eOdometer - target_apm) < 0.001)
    
    if (nrow(seg) == 0) {
       warning(paste("No LRS segment found for substation:", station_info$substation_ID,
                     "Route:", route, "APM:", target_apm, "Dir:", direction))
      return(NULL)
    } else {
      # If multiple boundary matches, take the first one
      # seg <- seg %>% slice(1)
      # Fraction is 0 if matching start, 1 if matching end
      frac <- ifelse(abs(target_apm - seg$bOdometer) < 0.001, 0, 1)
    }
  } else {
    # If multiple overlapping segments, take the first one
    if (nrow(seg) > 1) {
      # Reduced warning frequency
       warning(paste("Multiple LRS segments found for substation:", station_info$substation_ID, 
                    "- using first match."))
      seg <- seg %>% slice(1)
    }
    # Calculate fraction along the segment
    denominator <- seg$eOdometer - seg$bOdometer
    # Avoid division by zero if segment length is zero
    if (is.na(denominator) || denominator == 0) {
      frac <- 0 # Default to start if segment has no length
    } else {
      frac <- (target_apm - seg$bOdometer) / denominator
    }
  }
  
  # Process geometry
  geom <- st_geometry(seg) %>%
    st_line_merge() %>%
    st_cast("LINESTRING") # Ensure it's a single linestring
  
  # Use tryCatch for st_line_sample issues like invalid fraction
  pt <- tryCatch({
    # Ensure frac is valid before sampling
    if (!is.finite(frac) || frac < 0 || frac > 1) {
      # Reduced warning frequency
       warning(paste("Calculated fraction invalid for substation:", station_info$substation_ID, "Frac:", frac))
      return(NULL) 
    }
    
    st_line_sample(geom, sample = frac) %>%
      st_cast("POINT")
    
  }, error = function(e) {
     warning(paste("st_line_sample failed for substation:", station_info$substation_ID, 
                  "Route:", route, "APM:", target_apm, "Frac:", frac, "Error:", e$message))
    return(NULL) 
  })
  
  # Final check on the created point
  if (is.null(pt) || length(pt) == 0 || st_is_empty(pt)) {
    warning(paste("Could not generate point geometry for substation:", station_info$substation_ID))
    return(NULL)
  }
  
  return(data.table(substation_ID = station_info$substation_ID, geometry = pt))
}

# ==============================================================================
# PeMS Census Truck Data 
# ==============================================================================

col_names_census <- c("time", "station_ID", "substation_ID", "freeway_ID", 
                      "freeway_direction", "city_ID", "county_ID", "district_ID",
                      "abs_postmile", "station_type", "station_set_ID", "lane", 
                      "vehicle_class", "vehicle_count", "avg_speed", "violation_count",
                      "violation_code", "single_axle_count", "single_axle_count_duplicate",
                      "tandem_axle_count", "tridem_axle_count", "quad_axle_count",
                      "avg_gross_weight", "gross_weight_distribution", "avg_single_weight",
                      "avg_tandem_weight", "avg_tridem_weight", "avg_quad_weight",
                      "length_distribution", "avg_length", 
                      "avg_tandem_spacing", "avg_tridem_spacing", "avg_quad_spacing",
                      "avg_wheelbase", "wheelbase_distribution", "total_flex_esal_300",
                      "total_flex_esal_285", "total_rigid_esal_300", "total_rigid_esal_285"
) 

col_types_census <- c(
  time = "character", station_ID = "integer", substation_ID = "integer", freeway_ID = "character", 
  freeway_direction = "character", city_ID = "character", county_ID = "character", district_ID = "integer",
  abs_postmile = "numeric", station_type = "character", station_set_ID = "integer", lane = "integer", 
  vehicle_class = "integer", vehicle_count = "integer", avg_speed = "numeric", violation_count = "integer",
  violation_code = "character", single_axle_count = "integer", single_axle_count_duplicate = "integer",
  tandem_axle_count = "integer", tridem_axle_count = "integer", quad_axle_count = "integer",
  avg_gross_weight = "numeric", gross_weight_distribution = "character", avg_single_weight = "numeric",
  avg_tandem_weight = "numeric", avg_tridem_weight = "numeric", avg_quad_weight = "numeric",
  length_distribution = "numeric", avg_length = "character", 
  avg_tandem_spacing = "numeric", avg_tridem_spacing = "numeric", avg_quad_spacing = "numeric",
  avg_wheelbase = "numeric", wheelbase_distribution = "character", total_flex_esal_300 = "numeric",
  total_flex_esal_285 = "numeric", total_rigid_esal_300 = "numeric", total_rigid_esal_285 = "numeric"
)

# ==============================================================================
# Data Processing Function 
# ==============================================================================
start_year <- 2011
end_year <- 2011
##############################################################################
## Imports, combines, and locates PeMS truck census data for a given period.
##############################################################################

# Define Paths 
census_data_dir <- here::here("data-raw", "traffic_census")
lrs_shapefile_path <- here::here("data-raw", "State_Highway_Network_Lines", "State_Highway_Network_Lines.shp")

# Check Paths
if (!dir.exists(census_data_dir)) stop("PeMS data directory not found: ", census_data_dir)
if (!file.exists(lrs_shapefile_path)) stop("LRS shapefile not found: ", lrs_shapefile_path)

# Find Files
all_txt_files <- list.files(path = census_data_dir, 
                            pattern = "^all_text_tmg_trucks_hour_\\d{4}_\\d{2}_\\d{2}\\.txt$", 
                            full.names = TRUE)
if (length(all_txt_files) == 0) {
  warning("No PeMS .txt files found in: ", census_data_dir, ". Returning NULL for PeMS data.")
  return(NULL) # Return NULL if no files found
}

file_dates <- ymd(str_extract(basename(all_txt_files), "\\d{4}_\\d{2}_\\d{2}"))
file_years <- year(file_dates)
files_to_process <- all_txt_files[file_years >= start_year & file_years <= end_year]

n_files <- length(files_to_process)
if (n_files == 0) {
  warning("No PeMS data files found for years: ", start_year, "-", end_year, ". Returning NULL for PeMS data.")
  return(NULL) # Return NULL if no files found for the period
}
message("Found ", n_files, " PeMS files for ", start_year, "-", end_year, ".")

# Import and Combine
message("Importing and combining PeMS data...")
pb_read <- progress_bar$new(format = "  Reading PeMS census [:bar] :percent eta: :eta (:file)", total = n_files, clear = FALSE, width = 60)


all_data_list <- lapply(files_to_process, function(f) {
  pb_read$tick(tokens = list(file = basename(f)))
  tryCatch({
    # Note that some files are not consistently formatted
    # different observations have different number of columns
    # It is mysterious what the columns beyond the first 39's are 
    # but we will not be using them anyway
    # the first 39 cols looks good!
    
    # Strategy:
    # We use fill = Inf to force fread to read the entire file first
    # to find the maximum number of columns, preventing errors on long lines.
    # we will only keep the first 39 columns.
      dt_raw <- fread(f,
                      sep = ",",
                      header = FALSE, # Assume no header
                      showProgress = FALSE,
                      integer64="character", # Read potentially large IDs as character first
                      fill = Inf             # IMPORTANT: Find max cols first, then fill shorter rows with NA
      )
      
      # Check if any columns were read
      if (ncol(dt_raw) == 0) {
        warning("File ", basename(f), " appears empty or could not be parsed. Skipping file.", call. = FALSE)
        return(NULL)
      }
      
      
      # Select only the first 39 columns 
      dt <- dt_raw[, 1:39]
      
      # Assign the correct column names
      setnames(dt, col_names_census)
      
      # Apply the correct data types
      # Convert ID columns safely first
      id_cols <- c("station_ID", "substation_ID", "station_set_ID")
      for(col in id_cols) {
        # Use suppressWarnings for potential NAs introduced by coercion
        dt[, (col) := suppressWarnings(as.integer(get(col)))]
      }
      
      # Apply other types using the col_types_census list
      for(col in names(col_types_census)) {
        if (col %in% names(dt) && !(col %in% id_cols)) { # Avoid re-converting IDs
          target_type <- col_types_census[[col]]
          # Use suppressWarnings for potential NAs introduced by coercion
          if(target_type == "integer") dt[, (col) := suppressWarnings(as.integer(get(col)))]
          if(target_type == "numeric") dt[, (col) := suppressWarnings(as.numeric(get(col)))]
          # Character columns are likely already character, but explicitly ensure
          if(target_type == "character") dt[, (col) := as.character(get(col))]
        }
      }
      
      # Select only the columns needed for the final output
      dt[, .(time, station_ID, substation_ID, freeway_ID, freeway_direction,
             city_ID, county_ID, district_ID, abs_postmile, station_type,
             station_set_ID, lane, vehicle_class, vehicle_count)]
      
    }, error = function(e) {
    warning("Error reading PeMS file: ", basename(f), " - ", e$message, call. = FALSE)
    return(NULL)
  })
})

combined_data <- rbindlist(Filter(Negate(is.null), all_data_list))
if (nrow(combined_data) == 0) {
  warning("No PeMS data successfully read for ", start_year, "-", end_year, ". Returning NULL for PeMS data.")
  return(NULL) # Return NULL if reading failed
}
message("PeMS data import complete. Total rows: ", nrow(combined_data))

# In 2011 data, two substations (8878001 and 8878005) have freeway_ID = 944
# I believe this is a sloppy entry, there is no highway 944
# They are also not freeway_ID = 94 (wichih run west - east). since data say are NB or SB
# Lack of meta data, we are unable to locate them
# SO we delete them from our data

initial_rows <- nrow(combined_data)
combined_data <- combined_data[!(freeway_ID == "944" & freeway_direction %in% c("N", "S"))]
removed_rows <- initial_rows - nrow(combined_data)
if (removed_rows > 0) {
  message("Removed ", removed_rows, " rows with freeway_ID 944 and direction N or S due to inconsistency.")
}

# Identify Unique Locations 
message("Identifying unique PeMS substations...")
combined_data[, abs_postmile := as.numeric(abs_postmile)] 
unique_substations <- combined_data %>%
  filter(!is.na(substation_ID), !is.na(freeway_ID), !is.na(freeway_direction), !is.na(abs_postmile), abs_postmile >= 0) %>% # Add check for non-negative postmile
  distinct(substation_ID, station_ID, freeway_ID, freeway_direction, abs_postmile) %>%
  as.data.table()
message("Found ", nrow(unique_substations), " unique PeMS substation locations.")

if (nrow(unique_substations) == 0) {
  warning("No valid unique substations found to locate.")
  # Add Latitude/Longitude columns with NA if no locations can be found
  combined_data[, `:=`(Latitude = as.numeric(NA), Longitude = as.numeric(NA))]
  return(combined_data) # Return data without coordinates
}

# Load LRS
message("Loading LRS shapefile...")

lrs <- tryCatch({
  st_read(lrs_shapefile_path, quiet = TRUE) %>%
    mutate(RouteS = as.character(RouteS)) %>% 
    select(RouteS, Direction, bOdometer, eOdometer) 
}, error = function(e) {
  warning("Failed to load LRS shapefile: ", e$message, call. = FALSE)
  return(NULL)
})

if (is.null(lrs)) {
  warning("Cannot locate PeMS stations because LRS file failed to load.")
  combined_data[, `:=`(Latitude = as.numeric(NA), Longitude = as.numeric(NA))]
  return(combined_data) # Return data without coordinates
}
message("LRS data loaded.")

# Find Coordinates
message("Calculating PeMS coordinates...")
pb_loc <- progress_bar$new(format = "  Locating PeMS [:bar] :percent eta: :eta", total = nrow(unique_substations), clear = FALSE, width=60)

# Wrap lapply in tryCatch in case of widespread errors in get_point_from_lrs
coordinates_list <- tryCatch({
  lapply(split(unique_substations, seq(nrow(unique_substations))), function(row_dt) {
    pb_loc$tick()
    get_point_from_lrs(row_dt, lrs) 
  })
}, error = function(e) {
  warning("Error during coordinate calculation loop: ", e$message, call. = FALSE)
  return(list()) # Return empty list on major error
})

# Filter out NULLs more safely
valid_coordinates_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, coordinates_list)

if (length(valid_coordinates_list) == 0) {
  warning("Could not determine coordinates for any PeMS substations.")
  substation_coords_dt <- data.table(substation_ID = integer(), Latitude = numeric(), Longitude = numeric())
} else {

  substation_geometries <- dplyr::bind_rows(valid_coordinates_list)

  # convert to sf object to determine the lat and long
  substation_geometries_sf <- st_as_sf(substation_geometries, sf_column_name = "geometry")
  
  substation_geometries_wgs84 <- st_transform(substation_geometries_sf, 4326)
  coords <- st_coordinates(substation_geometries_wgs84)
  
  # Create the final coordinates table
  substation_coords_dt <- data.table(
    substation_ID = substation_geometries_wgs84$substation_ID,
    Latitude = coords[, "Y"],
    Longitude = coords[, "X"]
  )

}
# Report how many stations were successfully located
message(nrow(substation_coords_dt), " out of ", nrow(unique_substations), " unique PeMS locations geocoded.")

# Merge Coordinates
message("Merging PeMS coordinates...")
combined_data[, substation_ID := as.integer(substation_ID)]
substation_coords_dt[, substation_ID := as.integer(substation_ID)]
# Use data.table merge for efficiency
final_pems_data <- merge(combined_data, substation_coords_dt, by = "substation_ID", all.x = TRUE)
message("PeMS merge complete.")

# Check merge success rate
na_coords <- sum(is.na(final_pems_data$Latitude))
if (na_coords > 0) {
  warning(na_coords, " rows in the final PeMS data have missing coordinates.")
}

desired_order <- c(
  "district_ID", "city_ID", "county_ID", "station_set_ID", "station_ID", 
  "substation_ID", "station_type", "Latitude", "Longitude", "time", 
  "freeway_ID", "freeway_direction", "lane", "abs_postmile", 
  "vehicle_class", "vehicle_count"
)

setcolorder(final_pems_data, desired_order)


# ==============================================================================
# CARB Air Quality and Meteorological Information System data
# ==============================================================================
path <- here::here("data-raw", "CARB_AQMIS2_NOx")
start_year <- 2011
end_year <- 2011
years_to_match <- seq(start_year, end_year)

# Create a regex pattern from those years
# If years_to_match is c(2011, 2012), this creates: "(2011|2012)"
year_pattern <- paste(years_to_match, collapse = "|") %>% 
  paste0("(", ., ")")


# This will match "NOX_PICKDATA_" followed by one of the years
data_pattern <- paste0("NOX_PICKDATA_", year_pattern)
sitelist_pattern <- paste0("NOX_SITELIST_", year_pattern)

nox_files <- list.files(path = path,
                        pattern = data_pattern, 
                        full.names = TRUE)

all_hourly_data <- map(nox_files, ~ read_csv(., show_col_types = FALSE)) %>%
                   list_rbind() # There will be warnings because each of the data
                                # file has some notes about "Quality Flag Definition"
                                # that do not match the previous rows' pattern. 
                                # Don't worry about it.


all_hourly_data <- all_hourly_data %>%
  filter(!is.na(name)) %>% # This removes the rows about "Quality Flag Definition"
  mutate(quality = as.integer(quality)) %>%
  filter(!(quality %in% c(1,2,3,4,5))) %>% # This removes bad observations
  select(site, name, date, start_hour, variable, value, units) %>%
  mutate(
    site = as.integer(site),
    date = as.Date(date),
    start_hour = as.integer(start_hour),
    value = as.numeric(value)
  )


# load site list
sitelist_files <- list.files(path = path,
                             pattern = sitelist_pattern,
                             full.names = TRUE)

# Read and stack all sitelists
# There will be warnings because each of the data
# file has some notes about "Quality Flag Definition"
# that do not match the previous rows' pattern. 
# Don't worry about it.
all_sitelist_data <- map_dfr(
  sitelist_files, 
  ~ read_csv(., show_col_types = FALSE) 
)

# Create unique locations lookup table
nox_site_locations <- all_sitelist_data %>%
      filter(!is.na(site)) %>% # This removes the rows about "Quality Flag Definition"
      select(site, latitude, longitude) %>%
      distinct() %>% # Keep only one row for each unique site
      mutate(site = as.integer(site))

nox_data <- dplyr::left_join(all_hourly_data, nox_site_locations, by = "site")


################################################################################
#  Data export
################################################################################

# fwrite(nox_data, here("data-clean", "nox_2011.csv"), row.names = FALSE)
# fwrite(final_pems_data, here("data-clean", "pems_census_2011.csv"), row.names = FALSE)



################################################################################
#  Data from Georgia DOT
################################################################################

# Determine the sites we need
GA_annual <- read_csv(here::here("data-raw", "GA_data", "annualized_statistics.csv"))

functional_class <- unique(GA_annual$`Functional Class`)

functional_class

desired_functional_class <- c("1U : Urban Principal Arterial - Interstate",
                              "1R : Rural Principal Arterial - Interstate",
                              "2U : Urban Principal Arterial - Freeways & Expressways",
                              "3U : Urban Principal Arterial - Other",
                              "3R : Rural Principal Arterial - Other")

desired_list <- GA_annual %>%
  filter(`Functional Class` %in% desired_functional_class & `Station Type` == "CCS") %>%
  select(`Station ID`, `Functional Class`, `Latitude`, `Longitude`, `Statistics type`)


write.csv(desired_list, file = here("data-clean", "GA_monitors_desired_list.csv"), row.names = FALSE)




# By visual inspection, we can see verify we calculated the coordinates correctly
# First, they are all located on road, as pairs
# Second, if you are still concerned, use the WIM locations provided by Caltrans
# https://dot.ca.gov/programs/traffic-operations/wim/locations, Find the coordinates
# in https://postmile.dot.ca.gov/PMQT/PostmileQueryTool.html?, 
# and then verify our location using the following map

library(mapview)
mapview(substation_geometries_wgs84, zol = NULL, legend = FALSE, col.regions = "#2E8B57")











