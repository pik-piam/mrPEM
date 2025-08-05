#' Convert Read World Bank Carbon Pricing Dashboard data
#'
#' @md
#' @param x A [`magpie`][magclass::magclass] object returned from
#'          [`readWBCarbonPricingDashboard()`].
#' 
#' @param subtype character, type of data: price, price_1_april, coverage, revenue, emissions_share or emissions
#' 
#' @return A [`magpie`][magclass::magclass] object.
#'
#' @author Renato Rodrigues
#' 
#' @importFrom madrat calcOutput toolCountryFilln toolFillYears
#' @importFrom magclass dimSums getYears<- getYears mbind
#' @export
#'
convertWBCarbonPricingDashboard <- function(x, subtype) {

  if(subtype %in% c("emissions_share", "emissions")) {
    #correct emissions share data disaggregation for EU_ETS countries
    EU27 <- c("Austria" = "AUT", "Belgium" = "BEL", "Bulgaria" = "BGR", "Croatia" = "HRV", "Cyprus" = "CYP", "Czech Republic" = "CZE", "Denmark" = "DNK", "Estonia" = "EST", "Finland" = "FIN", "France" = "FRA", "Germany" = "DEU", "Greece" = "GRC", "Hungary" = "HUN", "Ireland" = "IRL", "Italy" = "ITA", "Latvia" = "LVA", "Lithuania" = "LTU", "Luxembourg" = "LUX", "Malta" = "MLT", "Netherlands" = "NLD", "Poland" = "POL", "Portugal" = "PRT", "Romania" = "ROU", "Slovakia" = "SVK", "Slovenia" = "SVN", "Spain" = "ESP", "Sweden" = "SWE")
    EU_ETS <- c(EU27, "Iceland" =	"ISL", "Liechtenstein" = "LIE", "Norway" = "NOR")

    emi.raw <- magclass::dimSums(madrat::readSource("EDGARghg"),3, na.rm = TRUE)

    #copying last year for missing years
    tmp <- tmp2 <- emi.raw[,2023,]
    magclass::getYears(tmp) <- 2024
    magclass::getYears(tmp2) <- 2025
    emi <- magclass::mbind(emi.raw,tmp,tmp2)[,magclass::getYears(x),]

    s <- emi[EU_ETS,,] / sum(emi[EU_ETS,,], na.rm = TRUE)
    x[EU_ETS,,] <- s[EU_ETS,,]*x[EU_ETS,,]

  }

  if (subtype == "emissions") {
    x <- x * magclass::dimSums(emi,1)
  }

  x[is.na(x)] <- 0 # setting NA values to zero
  x <- x[,sort(magclass::getYears(x, as.integer = T)),]
  x <- madrat::toolFillYears(x, sort(magclass::getYears(x, as.integer = T)))
  x <- madrat::toolCountryFill(x, fill = 0, verbosity = 2) # fill countries with no data
  

  return(x)
}
