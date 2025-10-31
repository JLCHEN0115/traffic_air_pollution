if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,  
  data.table, 
  sf,         
  lubridate,  
  progress,   
  here,
  mapview
)

################################################################################
# Match the PEMS traffic census monitors and the CARB NOx monitors
################################################################################
nox_file <- here::here("data-clean", "nox_2011.csv")
pems_file <- here::here("data-clean", "pems_census_2011.csv") 

message("Loading NOx data...")
nox_2011_dt <- fread(nox_file)

message("Loading PeMS census data...")
pems_census_2011_dt <- fread(pems_file)
message("PeMS census data loaded with ", nrow(pems_census_2011_dt), " rows.")

# ==========================================
# # Verify Lane 0  is the sum of other lanes
# ==========================================
# message("Verifying Lane 0 counts...")
# # Group by substation, time, and class, then sum counts for lane 0 and others
# check_lane0_dt <- pems_census_2011_dt[, .(
#   total_lane0 = sum(vehicle_count[lane == 0], na.rm = TRUE), # Sum lane 0
#   total_other = sum(vehicle_count[lane != 0], na.rm = TRUE)  # Sum other lanes
# ), by = .(substation_ID, time, vehicle_class)] # Grouping variables
# 
# # Calculate the difference
# check_lane0_dt[, diff := total_lane0 - total_other]
# 
# # Filter to find discrepancies (where diff is not zero)
# discrepancies <- check_lane0_dt[diff != 0]
# 
# # Report findings
# if (nrow(discrepancies) > 0) {
#   warning("Found ", nrow(discrepancies), " instances where Lane 0 count does not match the sum of other lanes.")
#   print("First few discrepancies:")
#   print(head(discrepancies))
# } else {
#   message("Lane 0 verification complete: All Lane 0 counts match the sum of other lanes.")
# }


message("Filtering PeMS data for Lane 0...")
# This creates a new data.table containing only lane 0 rows
pems_lane0_dt <- pems_census_2011_dt[lane == 0]
message("Filtered data contains ", nrow(pems_lane0_dt), " rows (Lane 0 only).")

message("Ready for matching PeMS and NOx data.")

# ===========================
#  Matching PeMS and NOx data
# ===========================

message("\n--- Starting PeMS to NOx Matching ---")

# Prepare NOx Data 
message("Preparing NOx monitor data...")
# Convert nox_2011_dt to data.table if it's not already
if (!is.data.table(nox_2011_dt)) {
  nox_2011_dt <- as.data.table(nox_2011_dt)
  message("Converted nox_2011_dt to data.table.")
}

# Create unique NOx monitor locations sf object
# Ensure lat/lon columns exist and are numeric
nox_coord_cols <- c("latitude", "longitude")
if (!all(nox_coord_cols %in% names(nox_2011_dt))) {
  stop("NOx data is missing required 'latitude' or 'longitude' columns.")
}
# Convert safely, coercing errors to NA
nox_2011_dt[, (nox_coord_cols) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = nox_coord_cols]

nox_sites_sf <- nox_2011_dt[!is.na(latitude) & !is.na(longitude), # Filter out missing coords
                            .(site = first(site), # Keep first site ID if duplicate coords exist
                              Latitude = first(latitude), 
                              Longitude = first(longitude)
                            ), by = .(latitude, longitude)] %>% # Get unique locations
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # Create sf object (WGS84)

if (nrow(nox_sites_sf) == 0) {
  stop("No valid NOx monitor locations found after processing coordinates.")
}
message("Prepared ", nrow(nox_sites_sf), " unique NOx monitor locations.")

# Prepare full NOx data for merging later (rename cols, create date/hour)
nox_merge_ready_dt <- nox_2011_dt[variable == "NOX", # Assuming you only want NOX
                                  .(site, 
                                    nox_date = as.Date(date), # Convert date string to Date
                                    nox_hour = as.integer(start_hour), # Ensure hour is integer
                                    NOx_value = suppressWarnings(as.numeric(value)) # Ensure value is numeric
                                  )]
# Remove rows where conversion failed
nox_merge_ready_dt <- nox_merge_ready_dt[!is.na(nox_date) & !is.na(nox_hour) & !is.na(NOx_value)] 
setkey(nox_merge_ready_dt, site, nox_date, nox_hour) # Set keys for faster merging
message("Prepared NOx data for time merging.")


# Prepare PeMS Data 
message("Preparing PeMS substation data...")
# Check if pems_lane0_dt exists and has rows
if (!exists("pems_lane0_dt") || is.null(pems_lane0_dt) || nrow(pems_lane0_dt) == 0) {
  stop("PeMS Lane 0 data ('pems_lane0_dt') not found or is empty.")
}
# Check for required columns
pems_req_cols <- c("substation_ID", "Latitude", "Longitude", "time")
if (!all(pems_req_cols %in% names(pems_lane0_dt))) {
  stop("PeMS data is missing required columns: ", paste(setdiff(pems_req_cols, names(pems_lane0_dt)), collapse=", "))
}

# Create unique PeMS substation locations sf object
pems_sites_sf <- pems_lane0_dt[!is.na(Latitude) & !is.na(Longitude), # Filter out missing coords
                               .(substation_ID = first(substation_ID)), # Keep first ID if duplicate coords
                               by = .(Latitude, Longitude)] %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # Create sf object (WGS84)

if (nrow(pems_sites_sf) == 0) {
  stop("No valid PeMS substation locations with coordinates found.")
}
message("Prepared ", nrow(pems_sites_sf), " unique PeMS substation locations with coordinates.")


# Find Nearest NOx Monitor for each PeMS Substation 
message("Calculating nearest NOx monitor for each PeMS substation...")

# Add progress bar for nearest neighbor calculation
n_pems_sites <- nrow(pems_sites_sf)
pb_nn <- progress_bar$new(format = "  Finding nearest neighbors [:bar] :percent eta: :eta", 
                          total = n_pems_sites, clear = FALSE, width = 60)

# Function to safely get nearest feature and distance for one PeMS site
find_nearest <- function(i, pems_sf, nox_sf) {
  pb_nn$tick()
  nearest_idx <- tryCatch(st_nearest_feature(pems_sf[i,], nox_sf), error = function(e) NA_integer_)
  if (is.na(nearest_idx)) return(data.table(nearest_nox_site = NA_integer_, distance_m = NA_real_))
  
  dist <- tryCatch(st_distance(pems_sf[i,], nox_sf[nearest_idx,], by_element = TRUE), error = function(e) NA_real_)
  
  return(data.table(
    nearest_nox_site = nox_sf$site[nearest_idx],
    distance_m = as.numeric(dist) # distance is in meters for WGS84
  ))
}

# Apply the function row-wise using lapply and combine results
nearest_neighbor_list <- lapply(1:n_pems_sites, find_nearest, pems_sf = pems_sites_sf, nox_sf = nox_sites_sf)
nearest_neighbor_dt <- rbindlist(nearest_neighbor_list)

# Combine with the unique PeMS substation IDs
pems_nearest_map_dt <- cbind(pems_sites_sf[, "substation_ID"] %>% st_drop_geometry() %>% as.data.table(), 
                             nearest_neighbor_dt)

# Check how many failed
failed_nn <- sum(is.na(pems_nearest_map_dt$nearest_nox_site))
if (failed_nn > 0) {
  warning("Could not find nearest neighbor for ", failed_nn, " PeMS substations.")
}
message("Nearest neighbor calculation complete.")


# Prepare PeMS Data for Time Merge
message("Preparing PeMS data time components...")
# Parse PeMS timestamp (MM/DD/YYYY HH:MM:SS)
# Use data.table's fast parsing `as.IDate` and `hour`
pems_lane0_dt[, pems_datetime := mdy_hms(time, tz = "UTC", quiet = TRUE)] # Parse full timestamp safely
pems_lane0_dt <- pems_lane0_dt[!is.na(pems_datetime)] # Remove rows where parsing failed

# Extract Date and Hour components
pems_lane0_dt[, `:=`(
  pems_date = as.IDate(pems_datetime), # Fast date conversion
  pems_hour = hour(pems_datetime)      # Extract hour (0-23)
)]
message("PeMS time components extracted.")


# Merge Nearest Neighbor Info into PeMS Data
message("Merging nearest monitor info into PeMS data...")
# Ensure substation_ID types match before merge
pems_lane0_dt[, substation_ID := as.integer(substation_ID)]
pems_nearest_map_dt[, substation_ID := as.integer(substation_ID)]

# Merge using data.table 
pems_with_nearest_dt <- merge(pems_lane0_dt, 
                              pems_nearest_map_dt[, .(substation_ID, nearest_nox_site, distance_m)], 
                              by = "substation_ID", 
                              all.x = TRUE) # Keep all PeMS rows
message("Nearest monitor info merged.")


#  Final Merge: PeMS with NOx Data 
message("Merging PeMS data with NOx data by nearest site and time...")
# Prepare keys for merging
setkey(pems_with_nearest_dt, nearest_nox_site, pems_date, pems_hour)
# nox_merge_ready_dt already keyed by site, nox_date, nox_hour

# Perform the merge (left join: keep all PeMS rows, add NOx where matches exist)
# Rename NOx columns during merge to avoid conflicts
final_merged_dt <- merge(pems_with_nearest_dt, 
                         nox_merge_ready_dt, 
                         by.x = c("nearest_nox_site", "pems_date", "pems_hour"), 
                         by.y = c("site", "nox_date", "nox_hour"), 
                         all.x = TRUE)

# Rename the merged NOx value column
setnames(final_merged_dt, "NOx_value", "NOx_nearest", skip_absent = TRUE)
message("Final merge complete. Result has ", nrow(final_merged_dt), " rows.")

# Clean up and Final Result 
# Remove intermediate columns 
final_merged_dt[, `:=`(pems_datetime = NULL, time = NULL)] 

# convert distance to miles
meters_to_miles_factor <- 0.000621371
final_merged_dt[, distance_miles := distance_m * meters_to_miles_factor]
final_merged_dt[, distance_m := NULL]

# View the structure and head of the final merged data
message("Final Merged Data Structure:")
str(final_merged_dt)
message("Final Merged Data Head:")
print(head(final_merged_dt)) # Now includes distance_miles

message("\n--- PeMS STATION to NOx Matching Complete ---")

# The final result is in the data.table 'final_merged_dt'
# It contains your aggregated PeMS station data, plus:
# - nearest_nox_site: The site ID of the closest NOx monitor
# - distance_m: The distance (in meters) to that monitor (if not removed)
# - distance_miles: The distance (in miles) to that monitor
# - NOx_nearest: The NOx value from that monitor for the matching hour (NA if no match)

summary(final_merged_dt$distance_miles)






# Mapview of monitor locations 
# Add a 'type' column to each sf object
pems_map_data <- pems_sites_sf
pems_map_data$type <- "PeMS Station" # Assign a category label

nox_map_data <- nox_sites_sf
nox_map_data$type <- "NOx Monitor"   # Assign a category label

mapview(pems_map_data,
        zcol = "type",            
        col.regions = "#56B4E9",   
        layer.name = "PeMS Stations") +
  mapview(nox_map_data,
          zcol = "type",            
          col.regions = "#D55E00",    
          layer.name = "NOx Monitors")










  