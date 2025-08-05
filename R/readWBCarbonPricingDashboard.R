#' Read World Bank Carbon Pricing Dashboard data
#'
#' This data can be downloaded directly from the site.
#'
#' @param subtype character, type of data: price, price_1_april, coverage, revenue, emissions_share or emissions
#' @returns MagPIE object with world banck data on carbon price per country
#'
#' @author Renato Rodrigues
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select all_of matches mutate left_join across filter %>% rename_with join_by 
#' bind_rows case_when .data
#' @importFrom tidyr extract pivot_longer
#' @importFrom magclass as.magpie
#' @importFrom madrat toolCountryFill toolGetMapping
#' @export
#' 
readWBCarbonPricingDashboard <- function(subtype = "price") {

  dashboardFile <- "data_2025.xlsx"

  raw.info <- suppressMessages(readxl::read_excel(dashboardFile, sheet = "Compliance_Gen Info", skip = 1, .name_repair = "unique")) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  raw.emissions <- readxl::read_excel(dashboardFile, sheet = "Compliance_Emissions", skip = 2) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  raw.revenue <- readxl::read_excel(dashboardFile, sheet = "Compliance_Revenue", skip = 1) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))
  raw.price <- readxl::read_excel(dashboardFile, sheet = "Compliance_Price", skip = 1) %>%
    rename_with(~gsub(" ", "_", tolower(.x)))

  wb.regionMapping <- toolGetMapping("wbRegion.csv", type = "regional", where = "mrPEM")

  wb.sectoralMapping <- toolGetMapping("wbSector.csv", type = "sectoral", where = "mrPEM")

  d.metadata <- raw.info  %>%
    left_join(wb.regionMapping, by = "unique_id") %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    dplyr::select(
      c("unique_id", "region", "region_type", "instrument_name", "type", "status", "jurisdiction_covered", "gases_covered", "sector_group", "electricity_and_heat", "industry",
        "mining_and_extractives", "transport", "aviation", "buildings", "agriculture,_forestry_and_fishing_fuel_use",
        "agricultural_emissions", "waste", "lulucf", "fuels_covered", "allocation_approaches", "price_or_market_management",
        "point_of_regulation", "offset_eligibility", "description", "recent_developments", "coverage", "pricing_and_allocation", "compliance", "relation_to_other_instruments")) %>%
    dplyr::left_join(
      raw.price %>%
        dplyr::select(c("unique_id", "income_group"))
      , by = "unique_id") %>%
    dplyr::mutate(
      type = case_when(
        type == "Carbon tax" ~ "carbon_tax",
        type == "ETS"        ~ "ets",
        type == "Undecided"  ~ NA,
        type == "Carbon Tax" ~ "carbon_tax"),
      status = case_when(
        status == "Implemented"         ~ "implemented",
        status == "Under consideration" ~ "under_consideration",
        status == "Abolished"           ~ "abolished",
        status == "Under development"   ~ "under_development")
    )

  d.coverage <- raw.info  %>%
    left_join(wb.regionMapping, by = "unique_id") %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "sector_group", "share_of_jurisdiction_emissions_covered")) %>%
    tidyr::extract(
      .data$share_of_jurisdiction_emissions_covered,
      into = c("emissions_coverage", "global_emissions"),
      regex = "(\\d+\\.?\\d*)% of jurisdiction emissions, (\\d+\\.?\\d*)% of global emissions",
      remove = TRUE
    ) %>%
    left_join(d.metadata %>% select(c("unique_id", "type", "status")) , by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "emissions_coverage")) %>%
    dplyr::mutate(emissions_coverage = as.numeric(.data$emissions_coverage) / 100) %>%
    filter(!(is.na(.data$emissions_coverage)))

  d.price_1_april <- raw.info  %>%
    left_join(wb.regionMapping, by = "unique_id")  %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group", "price_on_1_april")) %>%
    tidyr::extract(
      .data$price_on_1_april,
      into = c("price_on_1_april_US", "price_on_1_april_local_currency"),
      regex = "US\\$\\s*([0-9.,]+) \\(([^)]*)\\)",
      remove = TRUE
    ) %>%
    select(c("unique_id", "region", "region_type", "type", "status", "sector_group", "price_on_1_april_US")) %>%
    filter(!(is.na(.data$price_on_1_april_US)))

  d.emissionsShare <- raw.emissions %>%
    dplyr::select(c("name_of_the_initiative", all_of(names(raw.emissions)[suppressWarnings(!is.na(as.numeric(names(raw.emissions))))]))) %>%
    dplyr::left_join(d.metadata %>%
                      dplyr::select(c("unique_id", "instrument_name"))
                    , by = join_by("name_of_the_initiative" == "instrument_name")) %>%
    left_join(wb.regionMapping, by = "unique_id") %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    left_join(d.metadata %>% select(c("unique_id", "type", "status")) , by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group", all_of(names(raw.emissions)[suppressWarnings(!is.na(as.numeric(names(raw.emissions))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"), names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))

  d.revenue <- raw.revenue %>%
    dplyr::select(c("instrument_name", all_of(names(raw.revenue)[suppressWarnings(!is.na(as.numeric(names(raw.revenue))))]))) %>%
    dplyr::left_join(d.metadata %>%
                      dplyr::select(c("unique_id", "instrument_name"))
                    , by = "instrument_name") %>%
    left_join(wb.regionMapping, by = "unique_id") %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    left_join(d.metadata %>% select(c("unique_id", "type", "status")) , by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group", all_of(names(raw.revenue)[suppressWarnings(!is.na(as.numeric(names(raw.revenue))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"), names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))

  d.price <- raw.price %>%
    dplyr::select(c("unique_id", all_of(names(raw.price)[suppressWarnings(!is.na(as.numeric(names(raw.price))))]))) %>%
    left_join(wb.regionMapping, by = "unique_id") %>%
    left_join(wb.sectoralMapping, by = "unique_id") %>%
    left_join(d.metadata %>% select(c("unique_id", "type", "status")) , by = "unique_id") %>%
    dplyr::select(c("unique_id", "region", "region_type", "type", "status", "sector_group", all_of(names(raw.price)[suppressWarnings(!is.na(as.numeric(names(raw.price))))]))) %>%
    pivot_longer(-c("unique_id", "region", "region_type", "type", "status", "sector_group"), names_to = "period", values_to = "value") %>%
    filter(!(is.na(.data$value)))
  
  switch(subtype,
    "price" = { data <- d.price },
    "revenue" = { data <- d.revenue },
    "emissions_share" = { data <- d.emissionsShare },
    "emissions" = { data <- d.emissionsShare },
    "price_1_april" = { data <- d.price_1_april })

  EU27 <- c("Austria" = "AUT", "Belgium" = "BEL", "Bulgaria" = "BGR", "Croatia" = "HRV", "Cyprus" = "CYP", "Czech Republic" = "CZE", "Denmark" = "DNK", "Estonia" = "EST", "Finland" = "FIN", "France" = "FRA", "Germany" = "DEU", "Greece" = "GRC", "Hungary" = "HUN", "Ireland" = "IRL", "Italy" = "ITA", "Latvia" = "LVA", "Lithuania" = "LTU", "Luxembourg" = "LUX", "Malta" = "MLT", "Netherlands" = "NLD", "Poland" = "POL", "Portugal" = "PRT", "Romania" = "ROU", "Slovakia" = "SVK", "Slovenia" = "SVN", "Spain" = "ESP", "Sweden" = "SWE")
  EU_ETS <- c(EU27, "Iceland" =	"ISL", "Liechtenstein" = "LIE", "Norway" = "NOR")

  data <- bind_rows(
    data %>% dplyr::filter(.data$region_type == "country"),
    data %>%
      dplyr::filter(.data$unique_id == "ETS_EU") %>%
      dplyr::select(-c("region")) %>%
      tidyr::crossing(region = EU_ETS) %>%
      dplyr::mutate(region_type = "country") %>%
      select(all_of(names(data)))
    ) %>%
    dplyr::select(-c("unique_id", "region_type")) %>%
    as.magpie(spatial = "region", temporal = "period", datacol = "value")

  # Warning emissions and emissions shares require the convert function code to provide the correct output
  return(data)
}
