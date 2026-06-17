# R/rinex_reader.R
# Functions for reading RINEX navigation files

#' Read Klobuchar parameters from RINEX navigation file
#'
#' @param nav_file Path to RINEX navigation file (.nav)
#' @return List with alpha and beta parameters
#'
read_klobuchar_from_rinex <- function(nav_file) {
  
  # Read all lines
  lines <- readLines(nav_file)
  
  # Initialize parameters
  alpha <- c(0, 0, 0, 0)
  beta <- c(0, 0, 0, 0)
  
  # Search for ION ALPHA and ION BETA lines
  for (line in lines) {
    if (grepl("ION ALPHA", line)) {
      # Extract numbers from the line
      nums <- as.numeric(unlist(regmatches(line, gregexpr("-?[0-9.]+[Ee]?[+-]?[0-9]*", line))))
      if (length(nums) >= 4) {
        alpha <- nums[1:4]
      }
    }
    if (grepl("ION BETA", line)) {
      nums <- as.numeric(unlist(regmatches(line, gregexpr("-?[0-9.]+[Ee]?[+-]?[0-9]*", line))))
      if (length(nums) >= 4) {
        beta <- nums[1:4]
      }
    }
  }
  
  # Check if parameters were found
  if (all(alpha == 0) || all(beta == 0)) {
    warning("Klobuchar parameters not found in navigation file. Using default values.")
    # Fallback to example values (for 1995-10-15)
    alpha <- c(2.5e-8, 1.2e-8, -5.3e-8, 1.1e-8)
    beta <- c(9.5e4, 1.3e5, -6.5e4, 1.9e4)
  }
  
  return(list(alpha = alpha, beta = beta))
}

#' Read date from RINEX navigation file
#'
#' @param nav_file Path to RINEX navigation file
#' @return POSIXct date
#'
read_rinex_date <- function(nav_file) {
  lines <- readLines(nav_file, n = 5)
  
  # RINEX 2.x header contains date in format: YYYY MM DD HH MM SS
  # Look for line with date pattern
  for (line in lines) {
    # Try to find date pattern (4 digits year, 2 digits month, 2 digits day)
    date_match <- regexpr("[0-9]{4}\\s+[0-9]{2}\\s+[0-9]{2}", line)
    if (date_match != -1) {
      date_str <- regmatches(line, date_match)
      date_parts <- as.numeric(strsplit(date_str, "\\s+")[[1]])
      if (length(date_parts) == 3) {
        return(as.POSIXct(paste(date_parts[1], date_parts[2], date_parts[3], 
                                "00:00:00"), tz = "UTC"))
      }
    }
  }
  
  # If no date found, return current date
  return(as.POSIXct(Sys.Date(), tz = "UTC"))
}