# Write me a function that converts caliper to DBH
caliper_to_diameter  <- function (caliper) {
    DBH <- caliper / pi * 2.54
  return(DBH)
}

# dbh in cm to caliper in inches
diameter_to_caliper <- function (diameter) {
  caliper <- diameter / 2.54 * pi
  return(caliper)
}