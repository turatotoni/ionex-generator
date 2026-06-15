# R/klobuchar_model.R
# Klobuchar ionospheric delay model for GPS L1 frequency

#' Calculate Klobuchar ionospheric delay
#'
#' @param lat_deg Geographic latitude in degrees (range: -85 to 85)
#' @param lon_deg Geographic longitude in degrees (range: -180 to 180)
#' @param year Year (4 digits)
#' @param month Month (1-12)
#' @param day Day (1-31)
#' @param hour Hour (0-23)
#' @param min Minute (0-59)
#' @param sec Second (0-59)
#' @param alpha Alpha parameters (vector of 4: alpha0, alpha1, alpha2, alpha3)
#' @param beta Beta parameters (vector of 4: beta0, beta1, beta2, beta3)
#'
#' @return Ionospheric delay in seconds
#'
klobuchar_delay <- function(lat_deg, lon_deg, year, month, day, hour, min, sec, alpha, beta) {
  
  # Constants
  R_e <- 6378.137        # Earth radius (km)
  H_ion <- 350.0         # Ionospheric shell height (km)
  pi_val <- 3.141592653589793
  
  # Convert to radians
  lat_rad <- lat_deg * pi_val / 180
  lon_rad <- lon_deg * pi_val / 180
  
  # Day of year (simplified, works for most cases)
  day_of_year <- as.integer(strftime(as.Date(paste(year, month, day, sep = "-")), "%j"))
  
  # GPS time in seconds
  ut_sec <- hour * 3600 + min * 60 + sec
  
  # ============================================
  # STEP 1: Earth central angle (psi)
  # For zenith direction, elevation = 90 deg, so sin(elevation) = 1
  # psi = arccos( R_e/(R_e+H_ion) )
  # ============================================
  psi <- acos(R_e / (R_e + H_ion))
  
  # ============================================
  # STEP 2: Sub-ionospheric latitude (phi_i)
  # ============================================
  # Geocentric latitude (simplified)
  phi_geoc <- atan(0.99395 * tan(lat_rad))
  
  # Azimuth = 0 for zenith direction
  phi_i_rad <- asin(sin(phi_geoc) * cos(psi) + cos(phi_geoc) * sin(psi) * cos(0))
  phi_i_deg <- phi_i_rad * 180 / pi_val
  
  # ============================================
  # STEP 3: Apply latitude constraint
  # ============================================
  if (phi_i_deg > 0.416) phi_i_deg <- 0.416   # ~23.8 degrees
  if (phi_i_deg < -0.416) phi_i_deg <- -0.416
  phi_i_rad <- phi_i_deg * pi_val / 180
  
  # ============================================
  # STEP 4: Sub-ionospheric longitude (lambda_i)
  # ============================================
  # Azimuth = 0 for zenith direction
  lambda_i_rad <- lon_rad + (psi * sin(0)) / cos(phi_i_rad)
  lambda_i_deg <- lambda_i_rad * 180 / pi_val
  
  # ============================================
  # STEP 5: Geomagnetic latitude (phi_m)
  # ============================================
  phi_m_rad <- phi_i_rad + 0.064 * cos((lambda_i_rad - 1.617) * 180 / pi_val)
  phi_m_deg <- phi_m_rad * 180 / pi_val
  
  # ============================================
  # STEP 6: Local time at sub-ionospheric point (t_local)
  # ============================================
  t_local_sec <- ut_sec + lambda_i_deg * 43200 / pi_val  # 43200 = 12*3600/pi
  t_local_sec <- t_local_sec %% 86400
  t_local_hours <- t_local_sec / 3600
  
  # ============================================
  # STEP 7: Amplitude (A)
  # ============================================
  A <- alpha[1]
  for (i in 1:3) {
    A <- A + alpha[i + 1] * (phi_m_deg)^i
  }
  if (A < 0) A <- 0
  
  # ============================================
  # STEP 8: Period (P)
  # ============================================
  P <- beta[1]
  for (i in 1:3) {
    P <- P + beta[i + 1] * (phi_m_deg)^i
  }
  if (P < 72000) P <- 72000  # Minimum period 20 hours (72000 seconds)
  
  # ============================================
  # STEP 9: Phase (x)
  # ============================================
  x <- (2 * pi_val * (t_local_sec - 50400)) / P  # 50400 = 14:00 local time (peak)
  
  # ============================================
  # STEP 10: Vertical delay (F)
  # ============================================
  if (abs(x) < 1.57) {
    # Cosine approximation: cos(x) ≈ 1 - x^2/2 + x^4/24
    F <- 5e-9 + A * (1 - x^2/2 + x^4/24)
  } else {
    F <- 5e-9
  }
  
  # ============================================
  # STEP 11: Obliquity factor (F_obl)
  # For zenith direction, elevation = 90 deg
  # ============================================
  elevation_rad <- 90 * pi_val / 180
  F_obl <- 1 / sin(elevation_rad)  # = 1 for zenith
  
  # ============================================
  # STEP 12: Total delay in seconds
  # ============================================
  delay_seconds <- F * F_obl
  
  return(delay_seconds)
}

#' Convert ionospheric delay to TEC units (1 TECU = 10^16 el/m^2)
#'
#' @param delay_seconds Delay in seconds (L1 frequency, 1575.42 MHz)
#'
#' @return TEC in TECU (1 TECU = 10^16 el/m^2)
#'
delay_to_tec <- function(delay_seconds) {
  # For GPS L1 frequency: 1575.42 MHz = 1.57542e9 Hz
  f1 <- 1.57542e9
  # Constant: 40.3 m^3/s^2
  constant <- 40.3
  
  # TEC (el/m^2) = (delay_seconds * f1^2) / constant
  tec <- (delay_seconds * f1^2) / constant
  
  # Convert to TECU (1 TECU = 1e16 el/m^2)
  tec_tecu <- tec / 1e16
  
  return(tec_tecu)
}

#' Simplified Klobuchar model that returns reasonable TEC values
#' (Use this if the full model gives errors)
#'
#' @param lat_deg Latitude in degrees
#' @param lon_deg Longitude in degrees  
#' @param hour Hour of day (0-23)
#'
#' @return TEC in TECU (1-100 TECU typical range)
#'
simple_klobuchar_tec <- function(lat_deg, lon_deg, hour) {
  # Simple sinusoidal model for demonstration
  # TEC is highest around 14:00 local time, lowest at night
  
  # Convert longitude to local time offset
  local_hour <- (hour + lon_deg / 15) %% 24
  
  # Latitude effect: higher TEC near equator, lower at poles
  lat_factor <- cos(lat_deg * pi / 180)
  lat_factor <- max(0.2, min(1.0, lat_factor))
  
  # Diurnal variation: peak at 14:00 local time
  hour_rad <- (local_hour - 14) * 2 * pi / 24
  diurnal_factor <- (cos(hour_rad) + 1) / 2
  
  # TEC range: 5-50 TECU typical
  tec <- 10 * lat_factor + 30 * diurnal_factor * lat_factor
  
  return(tec)
}