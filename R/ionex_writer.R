# R/ionex_writer.R
# IONEX format writer for Version 1.1

#' Write IONEX header
#'
#' @param con File connection
#' @param param_file Parameter file path (contains alpha/beta)
#' @param lat1 First latitude (degrees)
#' @param lat2 Last latitude (degrees)
#' @param dlat Latitude increment (degrees)
#' @param lon1 First longitude (degrees)
#' @param lon2 Last longitude (degrees)
#' @param dlon Longitude increment (degrees)
#' @param height Ionospheric height (km)
#' @param nepochs Number of epochs
#' @param epoch_first First epoch as POSIXct
#' @param epoch_last Last epoch as POSIXct
#' @param interval Time interval between maps (seconds)
#'
write_ionex_header <- function(con, param_file, 
                               lat1 = 85.0, lat2 = -85.0, dlat = -5.0,
                               lon1 = 0.0, lon2 = 355.0, dlon = 5.0,
                               height = 400.0,
                               nepochs = 5,
                               epoch_first, epoch_last,
                               interval = 7200) {
  
  # IONEX VERSION / TYPE
  writeLines(sprintf("%8.1f%12s%1s%19s%3s%17s", 
                     1.1, "", "I", "GNSS", "", "IONEX VERSION / TYPE"), con)
  
  # PGM / RUN BY / DATE
  writeLines(sprintf("%-20s%-20s%-20s%s", 
                     "R_Klobuchar_v1.0", "User", format(Sys.time(), "%d-%b-%y %H:%M"), 
                     "PGM / RUN BY / DATE"), con)
  
  # DESCRIPTION
  writeLines(sprintf("%-60s%s", 
                     "Klobuchar model ionospheric TEC maps", "DESCRIPTION"), con)
  
  # COMMENT lines
  writeLines(sprintf("%-60s%s", 
                     "Generated from broadcast Klobuchar parameters", "COMMENT"), con)
  writeLines(sprintf("%-60s%s", 
                     "TEC values in 0.1 TECU; 9999 = no value", "COMMENT"), con)
  
  # EPOCH OF FIRST MAP
  writeLines(sprintf("%6d%6d%6d%6d%6d%6d%24s%s", 
                     as.integer(format(epoch_first, "%Y")),
                     as.integer(format(epoch_first, "%m")),
                     as.integer(format(epoch_first, "%d")),
                     as.integer(format(epoch_first, "%H")),
                     as.integer(format(epoch_first, "%M")),
                     as.integer(format(epoch_first, "%S")),
                     "", "EPOCH OF FIRST MAP"), con)
  
  # EPOCH OF LAST MAP
  writeLines(sprintf("%6d%6d%6d%6d%6d%6d%24s%s", 
                     as.integer(format(epoch_last, "%Y")),
                     as.integer(format(epoch_last, "%m")),
                     as.integer(format(epoch_last, "%d")),
                     as.integer(format(epoch_last, "%H")),
                     as.integer(format(epoch_last, "%M")),
                     as.integer(format(epoch_last, "%S")),
                     "", "EPOCH OF LAST MAP"), con)
  
  # INTERVAL
  writeLines(sprintf("%6d%54s%s", interval, "", "INTERVAL"), con)
  
  # # OF MAPS IN FILE
  writeLines(sprintf("%6d%54s%s", nepochs, "", "# OF MAPS IN FILE"), con)
  
  # MAPPING FUNCTION
  writeLines(sprintf("%6s%54s%s", "COSZ", "", "MAPPING FUNCTION"), con)
  
  # ELEVATION CUTOFF
  writeLines(sprintf("%8.1f%52s%s", 20.0, "", "ELEVATION CUTOFF"), con)
  
  # OBSERVABLES USED
  writeLines(sprintf("%-60s%s", "Klobuchar model", "OBSERVABLES USED"), con)
  
  # # OF STATIONS
  writeLines(sprintf("%6d%54s%s", 0, "", "# OF STATIONS"), con)
  
  # # OF SATELLITES
  writeLines(sprintf("%6d%54s%s", 0, "", "# OF SATELLITES"), con)
  
  # SYS / #STA / #SAT
  writeLines(sprintf("%5s%6d%6d%42s%s", "G", 0, 0, "", "SYS / #STA / #SAT"), con)
  
  # BASE RADIUS
  writeLines(sprintf("%8.1f%52s%s", 6371.0, "", "BASE RADIUS"), con)
  
  # MAP DIMENSION (2D maps)
  writeLines(sprintf("%6d%54s%s", 2, "", "MAP DIMENSION"), con)
  
  # HGT1 / HGT2 / DHGT
  writeLines(sprintf("%6.1f%6.1f%6.1f%40s%s", height, height, 0.0, "", "HGT1 / HGT2 / DHGT"), con)
  
  # LAT1 / LAT2 / DLAT
  writeLines(sprintf("%6.1f%6.1f%6.1f%40s%s", lat1, lat2, dlat, "", "LAT1 / LAT2 / DLAT"), con)
  
  # LON1 / LON2 / DLON
  writeLines(sprintf("%6.1f%6.1f%6.1f%40s%s", lon1, lon2, dlon, "", "LON1 / LON2 / DLON"), con)
  
  # EXPONENT (default -1 means 0.1 TECU)
  writeLines(sprintf("%6d%54s%s", -1, "", "EXPONENT"), con)
  
  # END OF HEADER
  writeLines(sprintf("%60s%s", "", "END OF HEADER"), con)
}

#' Write TEC data block for one epoch
#'
#' @param con File connection
#' @param epoch Current epoch as POSIXct
#' @param tec_matrix Matrix of TEC values (rows = latitudes, cols = longitudes)
#' @param latitudes Vector of latitudes
#' @param longitudes Vector of longitudes
#' @param height Ionospheric height (km)
#' @param exponent Exponent (default -1 means values * 10^1)
#'
write_tec_map <- function(con, epoch, tec_matrix, latitudes, longitudes, 
                          height = 400.0, exponent = -1) {
  
  # START OF TEC MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "START OF TEC MAP"), con)
  
  # EPOCH OF CURRENT MAP
  writeLines(sprintf("%6d%6d%6d%6d%6d%6d%24s%s", 
                     as.integer(format(epoch, "%Y")),
                     as.integer(format(epoch, "%m")),
                     as.integer(format(epoch, "%d")),
                     as.integer(format(epoch, "%H")),
                     as.integer(format(epoch, "%M")),
                     as.integer(format(epoch, "%S")),
                     "", "EPOCH OF CURRENT MAP"), con)
  
  # Write each latitude slice
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    # LAT/LON1/LON2/DLON/H record
    writeLines(sprintf("%6.1f%6.1f%6.1f%6.1f%6.1f%40s%s", 
                       lat, longitudes[1], longitudes[length(longitudes)], 
                       longitudes[2] - longitudes[1], height,
                       "", "LAT/LON1/LON2/DLON/H"), con)
    
    # Write TEC values (16 values per line)
    tec_row <- tec_matrix[i, ]
    n_vals <- length(tec_row)
    
    for (j in seq(1, n_vals, by = 16)) {
      end_idx <- min(j + 15, n_vals)
      values <- tec_row[j:end_idx]
      # Format: each value is I5 (5 characters, right-aligned)
      line <- ""
      for (val in values) {
        if (is.na(val) || val > 999.9) {
          line <- paste0(line, sprintf("%5d", 9999))
        } else {
          # Store as scaled integer (exponent -1 means multiply by 10)
          line <- paste0(line, sprintf("%5d", round(val * 10)))
        }
      }
      writeLines(line, con)
    }
  }
  
  # END OF TEC MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "END OF TEC MAP"), con)
}

#' Write simple RMS map (placeholder values)
#'
#' @param con File connection
#' @param epoch Current epoch as POSIXct
#' @param tec_matrix Matrix for dimensions (can use actual RMS or placeholder)
#' @param latitudes Vector of latitudes
#' @param longitudes Vector of longitudes
#' @param height Ionospheric height (km)
#'
write_rms_map <- function(con, epoch, tec_matrix, latitudes, longitudes, height = 400.0) {
  
  # START OF RMS MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "START OF RMS MAP"), con)
  
  # EPOCH OF CURRENT MAP
  writeLines(sprintf("%6d%6d%6d%6d%6d%6d%24s%s", 
                     as.integer(format(epoch, "%Y")),
                     as.integer(format(epoch, "%m")),
                     as.integer(format(epoch, "%d")),
                     as.integer(format(epoch, "%H")),
                     as.integer(format(epoch, "%M")),
                     as.integer(format(epoch, "%S")),
                     "", "EPOCH OF CURRENT MAP"), con)
  
  # Write each latitude slice with placeholder RMS values (20 = 2.0 TECU)
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    writeLines(sprintf("%6.1f%6.1f%6.1f%6.1f%6.1f%40s%s", 
                       lat, longitudes[1], longitudes[length(longitudes)], 
                       longitudes[2] - longitudes[1], height,
                       "", "LAT/LON1/LON2/DLON/H"), con)
    
    n_vals <- length(longitudes)
    for (j in seq(1, n_vals, by = 16)) {
      end_idx <- min(j + 15, n_vals)
      # Placeholder RMS = 20 (meaning 2.0 TECU with exponent -1)
      line <- paste(rep("   20", end_idx - j + 1), collapse = "")
      writeLines(line, con)
    }
  }
  
  # END OF RMS MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "END OF RMS MAP"), con)
}

#' Write empty height map (placeholder values)
#'
#' @param con File connection
#' @param epoch Current epoch as POSIXct
#' @param latitudes Vector of latitudes
#' @param longitudes Vector of longitudes
#' @param height Ionospheric height (km)
#'
write_height_map <- function(con, epoch, latitudes, longitudes, height = 400.0) {
  
  # START OF HEIGHT MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "START OF HEIGHT MAP"), con)
  
  # EPOCH OF CURRENT MAP
  writeLines(sprintf("%6d%6d%6d%6d%6d%6d%24s%s", 
                     as.integer(format(epoch, "%Y")),
                     as.integer(format(epoch, "%m")),
                     as.integer(format(epoch, "%d")),
                     as.integer(format(epoch, "%H")),
                     as.integer(format(epoch, "%M")),
                     as.integer(format(epoch, "%S")),
                     "", "EPOCH OF CURRENT MAP"), con)
  
  # Write each latitude slice with placeholder height values (0 = same as base)
  for (i in 1:length(latitudes)) {
    lat <- latitudes[i]
    
    writeLines(sprintf("%6.1f%6.1f%6.1f%6.1f%6.1f%40s%s", 
                       lat, longitudes[1], longitudes[length(longitudes)], 
                       longitudes[2] - longitudes[1], height,
                       "", "LAT/LON1/LON2/DLON/H"), con)
    
    n_vals <- length(longitudes)
    for (j in seq(1, n_vals, by = 16)) {
      end_idx <- min(j + 15, n_vals)
      # Height offset = 0 (meaning same as base radius)
      line <- paste(rep("    0", end_idx - j + 1), collapse = "")
      writeLines(line, con)
    }
  }
  
  # END OF HEIGHT MAP
  writeLines(sprintf("%6d%54s%s", 1, "", "END OF HEIGHT MAP"), con)
}