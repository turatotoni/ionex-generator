# R/main_generator.R
# Main script: Generate IONEX file from Klobuchar model

# Source the required R files
source("R/klobuchar_model.R")
source("R/ionex_writer.R")

# ============================================
# STEP 1: Define Klobuchar parameters
# ============================================
# These are example broadcast parameters (from typical navigation message)
# Format: alpha0, alpha1, alpha2, alpha3, beta0, beta1, beta2, beta3
# Units: alpha in seconds, beta in seconds

# Example parameters (can be replaced with actual broadcast values)
alpha <- c(2.5e-8, 1.2e-8, -5.3e-8, 1.1e-8)   # alpha0..alpha3
beta  <- c(9.5e4,  1.3e5,   -6.5e4,   1.9e4)   # beta0..beta3

# ============================================
# STEP 2: Define grid parameters
# ============================================
# Latitude: from 85° to -85° with step -5°
latitudes <- seq(85.0, -85.0, by = -5.0)

# Longitude: from 0° to 355° with step 5°
longitudes <- seq(0.0, 355.0, by = 5.0)

# Ionospheric height (km)
height <- 400.0

# ============================================
# STEP 3: Define epochs
# ============================================
# Example: 5 epochs on 15 October 1995 (as in IONEX example)
epochs <- c(
  as.POSIXct("1995-10-15 00:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 06:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 12:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 18:00:00", tz = "UTC"),
  as.POSIXct("1995-10-16 00:00:00", tz = "UTC")
)

# ============================================
# STEP 4: Generate TEC matrix for each epoch
# ============================================

cat("Generating TEC maps using Klobuchar model...\n")

# Pre-allocate list for TEC maps
tec_maps <- list()

for (k in 1:length(epochs)) {
  epoch <- epochs[k]
  cat(sprintf("  Processing epoch %d/%d: %s\n", k, length(epochs), format(epoch, "%Y-%m-%d %H:%M:%S")))
  
  # Extract date/time components
  year <- as.integer(format(epoch, "%Y"))
  month <- as.integer(format(epoch, "%m"))
  day <- as.integer(format(epoch, "%d"))
  hour <- as.integer(format(epoch, "%H"))
  minute <- as.integer(format(epoch, "%M"))
  second <- as.integer(format(epoch, "%S"))
  
  # Create matrix for TEC values (rows = latitudes, cols = longitudes)
  tec_matrix <- matrix(NA, nrow = length(latitudes), ncol = length(longitudes))
  
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    for (j in 1:length(longitudes)) {
      lon <- longitudes[j]
      
      # Compute Klobuchar delay in seconds
      delay_sec <- klobuchar_delay(
        lat_deg = lat,
        lon_deg = lon,
        year = year,
        month = month,
        day = day,
        hour = hour,
        min = minute,
        sec = second,
        alpha = alpha,
        beta = beta
      )
      
      # Convert delay to TECU (TEC units: 1 TECU = 10^16 el/m^2)
      tec <- delay_to_tec(delay_sec)
      
      # Store in matrix
      tec_matrix[i, j] <- tec
    }
  }
  
  tec_maps[[k]] <- tec_matrix
}

cat("TEC map generation complete!\n")

# ============================================
# STEP 5: Write IONEX file
# ============================================

output_file <- "data/example_output.ionex"
cat(sprintf("Writing IONEX file to: %s\n", output_file))

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Open connection
con <- file(output_file, open = "wt")

# Write header
write_ionex_header(
  con = con,
  param_file = NULL,
  lat1 = latitudes[1],
  lat2 = latitudes[length(latitudes)],
  dlat = latitudes[2] - latitudes[1],
  lon1 = longitudes[1],
  lon2 = longitudes[length(longitudes)],
  dlon = longitudes[2] - longitudes[1],
  height = height,
  nepochs = length(epochs),
  epoch_first = epochs[1],
  epoch_last = epochs[length(epochs)],
  interval = 21600  # 6 hours in seconds
)

# Write TEC maps, RMS maps, and Height maps for each epoch
for (k in 1:length(epochs)) {
  cat(sprintf("  Writing map %d/%d\n", k, length(epochs)))
  
  # Write TEC map
  write_tec_map(
    con = con,
    epoch = epochs[k],
    tec_matrix = tec_maps[[k]],
    latitudes = latitudes,
    longitudes = longitudes,
    height = height,
    exponent = -1
  )
  
  # Write RMS map (placeholder)
  write_rms_map(
    con = con,
    epoch = epochs[k],
    tec_matrix = tec_maps[[k]],
    latitudes = latitudes,
    longitudes = longitudes,
    height = height
  )
  
  # Write Height map (placeholder)
  write_height_map(
    con = con,
    epoch = epochs[k],
    latitudes = latitudes,
    longitudes = longitudes,
    height = height
  )
}

# Write END OF FILE
writeLines(sprintf("%60s%s", "", "END OF FILE"), con)

# Close connection
close(con)

cat("Done! IONEX file successfully created.\n")
cat(sprintf("Output file: %s\n", normalizePath(output_file)))