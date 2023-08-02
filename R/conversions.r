# Convert caliper measurement to diameter at breast height
#
# @param caliper A numeric vector of caliper measurements in inches
# @return The corresponding diameter at breast height in centimeters
caliper_to_diameter <- function(caliper) {
  DBH <- (caliper / pi) * 2.54
  return(DBH)
}

# Convert diameter at breast height to caliper measurement
#
# @param diameter A numeric vector of diameter measurements in centimeters
# @return The corresponding caliper measurement in inches
diameter_to_caliper <- function(diameter) {
  caliper <- (diameter / 2.54) * pi
  return(caliper)
}

# Convert carbon emissions to carbon dioxide equivalent emissions
#
# @param carbon_emissions A numeric vector of carbon emissions in the units corresponding to the chosen gas type
# @param gas_type A character vector of gas types
# @return The corresponding carbon dioxide equivalent emissions
# @link https://ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf
carbon_to_carbon_equivalents <- function(carbon_emissions, gas_type) {
  # Define conversion factors for different greenhouse gases
  conversion_factors <- c(
    CO2 = 1,           # Carbon dioxide
    CH4 = 28,          # Methane
    N2O = 265,         # Nitrous oxide
    CCl3F = 4660,      # CFC-11
    CCl2F2 = 10200,    # CFC-12
    CClF3 = 13900,     # CFC-13
    CCl2FCClF2 = 5820, # CFC-113
    CClF2CClF2 = 8590, # CFC-114
    CClF2CF3 = 7670,   # CFC-115
    CBrF3 = 6290,      # halon 1301
    CBrClF2 = 1750,    # halon 1211
    CBrF2CBrF2 = 1470  # halon 2402
  )

  # Calculate carbon equivalents based on gas type
  carbon_equivalents <- carbon_emissions * conversion_factors[gas_type]

  return(carbon_equivalents)
}
