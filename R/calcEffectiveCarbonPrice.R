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
#' @importFrom magclass dimReduce collapseNames
#' @export
#'
calcEffectiveCarbonPrice <- function() {

  d.price <- madrat::readSource("WBCarbonPricingDashboard", subtype = "price")
  d.emissions <- madrat::readSource("WBCarbonPricingDashboard", subtype = "emissions")

  # calculate effective carbon price
  d.effectivePrice <- magclass::dimReduce(mselect(d.price[,intersect(getYears(d.price),getYears(d.emissions)),intersect(getNames(d.price),getNames(d.emissions))] * d.emissions[,intersect(getYears(d.price),getYears(d.emissions)),intersect(getNames(d.price),getNames(d.emissions))],status="implemented"))

  # GDP per capita
  gdpPc <- calcOutput("GDPpc", scenario = "SSP2", aggregate = FALSE, supplementary = TRUE)
  w <- magclass::collapseNames(gdpPc$weight)

  return(list(x = d.effectivePrice,
              weight = w,
              min = 0,
              description = "Effective carbon price based on the World Bank dashboard data",
              unit = "US$2017/t CO2"))
}