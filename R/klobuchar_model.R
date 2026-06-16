#' Simplified Klobuchar model that returns realistic TEC values
#'
#' @param lat_deg Latitude in degrees (-85 to 85)
#' @param lon_deg Longitude in degrees (0 to 355)  
#' @param hour Hour of day (0-23)
#'
#' @return TEC in TECU (typical range: 5-80 TECU)
#'
simple_klobuchar_tec <- function(lat_deg, lon_deg, hour) {
  
  # Convert to radians
  lat_rad <- lat_deg * pi / 180
  
  # 1. Latitude effect: TEC highest near equator (0°), lowest at poles
  lat_factor <- cos(lat_rad)
  lat_factor <- max(0.1, min(1.0, lat_factor))  # Clamp between 0.1 and 1.0
  
  # 2. Local time (account for longitude)
  local_hour <- (hour + lon_deg / 15) %% 24
  
  # 3. Diurnal variation: peak at 14:00 local time, minimum at 4:00
  # Using cosine: max at 14:00 (cos(0) = 1), min at 4:00 (cos(pi) = -1)
  hour_rad <- (local_hour - 14) * 2 * pi / 24
  diurnal_factor <- (cos(hour_rad) + 1) / 2  # Range: 0 to 1
  
  # 4. Seasonal variation (simplified - using fixed day of year)
  # For October 15 (day ~288), slightly higher TEC
  day_factor <- 1.0  # Can be adjusted
  
  # 5. Combine to get realistic TEC values
  # Base TEC at equator during daytime: ~50 TECU
  # At night: ~10 TECU
  # At poles: ~5 TECU
  base_tec <- 5  # Minimum TEC at poles
  equatorial_tec <- 45  # Additional TEC at equator
  
  tec <- base_tec + equatorial_tec * lat_factor * (0.2 + 0.8 * diurnal_factor)
  
  # 6. Add small random variation for realism (optional)
  # tec <- tec + runif(1, -1, 1)
  
  # 7. Ensure values are within reasonable range
  tec <- max(1, min(80, tec))
  
  return(tec)
}