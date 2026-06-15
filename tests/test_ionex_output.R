# tests/test_ionex_output.R
# Basic validation of IONEX output file

test_ionex_file <- function(file_path) {
  
  cat("Testing IONEX file:", file_path, "\n")
  cat("========================================\n")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("ERROR: File does not exist!")
  }
  cat("✓ File exists\n")
  
  # Read first 10 lines
  lines <- readLines(file_path, n = 30)
  
  # Check header
  version_line <- lines[1]
  if (grepl("IONEX VERSION / TYPE", version_line)) {
    cat("✓ IONEX VERSION record found\n")
  } else {
    cat("✗ Missing IONEX VERSION record\n")
  }
  
  # Check for END OF HEADER
  eoh_line <- grep("END OF HEADER", lines)
  if (length(eoh_line) > 0) {
    cat("✓ END OF HEADER found\n")
  } else {
    cat("✗ END OF HEADER not found\n")
  }
  
  # Check for TEC maps
  tec_start <- grep("START OF TEC MAP", lines)
  if (length(tec_start) > 0) {
    cat(sprintf("✓ Found %d TEC map(s) in first 30 lines\n", length(tec_start)))
  } else {
    cat("✗ No TEC maps found\n")
  }
  
  # Check for END OF FILE
  full_file <- readLines(file_path)
  last_line <- full_file[length(full_file)]
  if (grepl("END OF FILE", last_line)) {
    cat("✓ END OF FILE found\n")
  } else {
    cat("✗ END OF FILE not found\n")
  }
  
  cat("\nFile size:", file.size(file_path), "bytes\n")
  cat("Number of lines:", length(full_file), "\n")
  cat("\n✅ Basic validation complete!\n")
}

# Run the test
test_ionex_file("data/example_output.ionex")