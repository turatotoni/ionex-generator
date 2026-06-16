# R/main_generator.R
# Main script: Generate IONEX file from Klobuchar model

# Source the required R files
source("R/klobuchar_model.R")
source("R/ionex_writer.R")

# ============================================
# STEP 1: Define grid parameters
# ============================================
latitudes <- seq(85.0, -85.0, by = -5.0)      # 35 latitude values
longitudes <- seq(0.0, 355.0, by = 5.0)       # 72 longitude values
height <- 400.0

# ============================================
# STEP 2: Define epochs
# ============================================
epochs <- c(
  as.POSIXct("1995-10-15 00:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 06:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 12:00:00", tz = "UTC"),
  as.POSIXct("1995-10-15 18:00:00", tz = "UTC"),
  as.POSIXct("1995-10-16 00:00:00", tz = "UTC")
)

# ============================================
# STEP 3: Generate TEC matrix for each epoch
# ============================================

cat("\n")
cat("========================================\n")
cat("Generating TEC maps using Klobuchar model\n")
cat("========================================\n\n")

tec_maps <- list()

for (k in 1:length(epochs)) {
  epoch <- epochs[k]
  cat(sprintf("Epoch %d/%d: %s\n", k, length(epochs), format(epoch, "%Y-%m-%d %H:%M:%S")))
  
  # Extract hour (0-23) for the model
  hour_of_day <- as.integer(format(epoch, "%H"))
  
  # Create matrix for TEC values
  tec_matrix <- matrix(NA, nrow = length(latitudes), ncol = length(longitudes))
  
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    for (j in 1:length(longitudes)) {
      lon <- longitudes[j]
      
      # Use simplified Klobuchar model
      tec <- simple_klobuchar_tec(lat, lon, hour_of_day)
      
      # Store in matrix
      tec_matrix[i, j] <- tec
    }
    
    # Progress indicator
    if (i %% 5 == 0) cat("  Progress:", round(i/length(latitudes)*100), "%\n")
  }
  
  tec_maps[[k]] <- tec_matrix
  cat(sprintf("  Epoch %d complete!\n\n", k))
}

cat("All TEC maps generated successfully!\n\n")

# ============================================
# STEP 4: Write IONEX file
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
  interval = 21600
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
    tec_matrix = tec_maps[[k]],
    latitudes = latitudes,
    longitudes = longitudes,
    height = height
  )
}

# Write END OF FILE
writeLines(sprintf("%60s%s", "", "END OF FILE"), con)

# Close connection
close(con)

cat("\n")
cat("========================================\n")
cat("SUCCESS! IONEX file created:\n")
cat(sprintf("  %s\n", normalizePath(output_file)))
cat(sprintf("  File size: %d bytes\n", file.size(output_file)))
cat("========================================\n")