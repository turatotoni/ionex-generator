# run.R
# Main entry point - run this script to generate IONEX file

cat("\n")
cat("========================================\n")
cat("IONEX Generator - Klobuchar Model\n")
cat("========================================\n")
cat("\n")

# Source and run main generator
source("R/main_generator.R")

cat("\n")
cat("========================================\n")
cat("To validate the output, run:\n")
cat("  source('tests/test_ionex_output.R')\n")
cat("========================================\n")