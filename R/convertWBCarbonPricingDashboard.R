#' Convert Read World Bank Carbon Pricing Dashboard data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readWBCarbonPricingDashboard()`].
#'
#' @param subtype character, type of data: price, priceAprilFirst, wbCoverage, revenue, emissions_covered
#'
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues
#'
#' @importFrom madrat calcOutput toolCountryFill toolFillYears
#' @importFrom magclass dimSums getYears<- getYears mbind
#' @export
#'
convertWBCarbonPricingDashboard <- function(x, subtype) {

  x[is.na(x)] <- 0 # setting NA values to zero
  x <- x[, sort(magclass::getYears(x, as.integer = TRUE)), ]
  x <- madrat::toolFillYears(x, sort(magclass::getYears(x, as.integer = TRUE)))
  x <- madrat::toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
