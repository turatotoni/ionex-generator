# R/main_generator.R
# Main script: Generate IONEX file from Klobuchar model
# Now reads real Klobuchar parameters from RINEX navigation file

# Source the required R files
source("R/klobuchar_model.R")
source("R/ionex_writer.R")
source("R/rinex_reader.R")  # New file

# ============================================
# STEP 0: Read Klobuchar parameters from RINEX file
# ============================================

# Path to your RINEX navigation file
# CHANGE THIS to your actual file path!
nav_file <- "data/brdc2880.95n"  # After decompression (.Z -> .n)

cat("\n")
cat("========================================\n")
cat("Reading Klobuchar parameters from RINEX\n")
cat("========================================\n\n")

# Check if file exists
if (file.exists(nav_file)) {
  cat(sprintf("Reading navigation file: %s\n", nav_file))
  
  # Read parameters
  klobuchar <- read_klobuchar_from_rinex(nav_file)
  alpha <- klobuchar$alpha
  beta <- klobuchar$beta
  
  cat("Alpha parameters:\n")
  cat(sprintf("  α0 = %.4e\n", alpha[1]))
  cat(sprintf("  α1 = %.4e\n", alpha[2]))
  cat(sprintf("  α2 = %.4e\n", alpha[3]))
  cat(sprintf("  α3 = %.4e\n", alpha[4]))
  cat("\n")
  cat("Beta parameters:\n")
  cat(sprintf("  β0 = %.4e\n", beta[1]))
  cat(sprintf("  β1 = %.4e\n", beta[2]))
  cat(sprintf("  β2 = %.4e\n", beta[3]))
  cat(sprintf("  β3 = %.4e\n", beta[4]))
  cat("\n")
  
  # Get date from RINEX file
  rinex_date <- read_rinex_date(nav_file)
  cat(sprintf("Date from RINEX file: %s\n", format(rinex_date, "%Y-%m-%d")))
  
  use_simple_model <- FALSE
  cat("\nUsing FULL Klobuchar model with real parameters.\n")
  
} else {
  cat(sprintf("WARNING: Navigation file not found: %s\n", nav_file))
  cat("Using simplified model with default parameters.\n\n")
  
  # Fallback to simplified model
  use_simple_model <- TRUE
  
  # Still use default date for epochs
  rinex_date <- as.POSIXct("1995-10-15 00:00:00", tz = "UTC")
}

# ============================================
# STEP 1: Define grid parameters
# ============================================
latitudes <- seq(85.0, -85.0, by = -5.0)      # 35 latitude values
longitudes <- seq(0.0, 355.0, by = 5.0)       # 72 longitude values
height <- 400.0

# ============================================
# STEP 2: Define epochs based on RINEX date
# ============================================
epoch_date <- as.Date(rinex_date)

epochs <- c(
  as.POSIXct(paste(epoch_date, "00:00:00"), tz = "UTC"),
  as.POSIXct(paste(epoch_date, "06:00:00"), tz = "UTC"),
  as.POSIXct(paste(epoch_date, "12:00:00"), tz = "UTC"),
  as.POSIXct(paste(epoch_date, "18:00:00"), tz = "UTC"),
  as.POSIXct(paste(epoch_date + 1, "00:00:00"), tz = "UTC")
)

cat(sprintf("Epochs generated for: %s\n", format(epoch_date, "%Y-%m-%d")))
cat(sprintf("First epoch: %s\n", format(epochs[1], "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Last epoch:  %s\n\n", format(epochs[length(epochs)], "%Y-%m-%d %H:%M:%S")))

# ============================================
# STEP 3: Generate TEC matrix for each epoch
# ============================================

cat("\n")
cat("========================================\n")
cat("Generating TEC maps\n")
cat("========================================\n\n")

tec_maps <- list()

for (k in 1:length(epochs)) {
  epoch <- epochs[k]
  cat(sprintf("Epoch %d/%d: %s\n", k, length(epochs), format(epoch, "%Y-%m-%d %H:%M:%S")))
  
  # Extract date/time components
  year <- as.integer(format(epoch, "%Y"))
  month <- as.integer(format(epoch, "%m"))
  day <- as.integer(format(epoch, "%d"))
  hour <- as.integer(format(epoch, "%H"))
  minute <- as.integer(format(epoch, "%M"))
  second <- as.integer(format(epoch, "%S"))
  
  # Create matrix for TEC values
  tec_matrix <- matrix(NA, nrow = length(latitudes), ncol = length(longitudes))
  
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    for (j in 1:length(longitudes)) {
      lon <- longitudes[j]
      
      if (use_simple_model) {
        # Use simplified model (for testing without RINEX)
        tec <- simple_klobuchar_tec(lat, lon, hour)
      } else {
        # Use full Klobuchar model with real parameters
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
        tec <- delay_to_tec(delay_sec)
      }
      
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

# Generate output filename based on date
output_file <- sprintf("data/ionex_%s.ionex", format(epoch_date, "%Y%m%d"))
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
  param_file = nav_file,
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
  
  # Write RMS map
  write_rms_map(
    con = con,
    epoch = epochs[k],
    tec_matrix = tec_maps[[k]],
    latitudes = latitudes,
    longitudes = longitudes,
    height = height
  )
  
  # Write Height map
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