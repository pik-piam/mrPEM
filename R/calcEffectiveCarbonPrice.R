#' Calculate the Effective Carbon Price based on the World Bank dashboard data
#'
#' Effective Carbon Price is calculated from world banck emissions share coverage,
#' EDGAR Global GHG Emissions and World Bank Carbon Price data.
#'
#' @returns MAgPIE object with Effective Carbon Price per country and sector group
#'
#' @author Renato Rodrigues
#'
#' @examples
#' \dontrun{
#' calcOutput("WBCarbonPricingDashboard", subtype = "price")
#' }
#'
#' @importFrom madrat readSource
#' @importFrom magclass dimReduce
#' @export
#'
calcEffectiveCarbonPrice <- function() {

  d.price <- magclass::dimReduce(madrat::readSource("WBCarbonPricingDashboard", subtype = "price")[,,"implemented"])
  d.emissions <- madrat::readSource("WBCarbonPricingDashboard", subtype = "emissions")

  # calculate effective carbon price
  d.effectivePrice <- d.price * d.emissions

  return(list(x = d.effectivePrice,
              weight = NULL,
              min = 0,
              description = "Effective carbon price based on the World Bank dashboard data",
              unit = "US$2017/t CO2"))

}