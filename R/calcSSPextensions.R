#' Calculate SSP Extensions data
#'
#' @return A list with a [`magpie`][magclass::magclass] object, weight, unit and description.
#' @author Renato Rodrigues
#'
#' @importFrom madrat readSource calcOutput toolTimeInterpolate
#' @importFrom magclass getYears getNames mbind setNames
#'
calcSSPextensions <- function() {

  data <- readSource("SSPextensions")

  # Prepare weights
  # Population for everything related to people (shares, indices, etc.)
  pop <- calcOutput("Population", scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"), aggregate = FALSE)
  # GDP for economic shares
  gdp <- calcOutput("GDP", scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"), aggregate = FALSE)

  # Common years
  years <- intersect(getYears(data), intersect(getYears(pop), getYears(gdp)))
  data <- data[, years, ]
  pop <- pop[, years, ]
  gdp <- gdp[, years, ]

  # Define variables for each weight category
  vars_sum <- c(
      "GDP|PPP [Conflict-Adjusted Projections]",
      "Net Migration",
      "Population|Extreme Poverty"
  )

  vars_gdp <- c(
      "Value Added|Agriculture [Share]",
      "Value Added|Industry [Share]",
      "Value Added|Services [Share]"
  )

  # systematic weighting by population for everything else
  # (Indices, Probabilities, Population Shares, Empoyment Shares, Remittances per capita)
  vars_pop <- setdiff(getNames(data), c(vars_sum, vars_gdp))

  # Construct weight object
  w_pop <- data
  w_pop[, , ] <- NA
  w_pop[, , vars_pop] <- pop

  w_gdp <- data
  w_gdp[, , ] <- NA
  w_gdp[, , vars_gdp] <- gdp

  # Combine weights (NA where weight should be NULL/1 for summation, but madrat expects explicit weights for weighted average)
  weight <- w_pop
  weight[, , vars_gdp] <- w_gdp[, , vars_gdp]
  # vars_sum remain NA

  return(list(
      x = data,
      weight = weight,
      unit = "various",
      description = "SSP Extensions data"
  ))
}
