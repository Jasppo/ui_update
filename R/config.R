# Nav & Tab States: Include/Exclude Navs and Tabs ========================================================================================== 

# Set any nav to FALSE to hide it from UI + server
include_navs <- list(
  homeID = TRUE, 
  mapsID = TRUE,
  ranksID = TRUE,
  trendsID = TRUE,
  disparitiesID = TRUE,
  demographicsID = TRUE,
  aboutID = TRUE
)

# Set any tab (subtab) to FALSE to hide it from UI + server
include_tabs <- list(
  mapsTab = TRUE,
  rankByCauseTab = TRUE,
  rankByGeoTab = TRUE,
  demoTrendTab = TRUE,
  lifeExpTab = FALSE, 
  topTrendsTab = FALSE,
  disparitiesTab = TRUE,
  demographicsTab = TRUE,
  techNotesTab = TRUE
)

# Set Years for data =============================================================================

current_years <- list(
  currentYear = 2021,
  currentYear_hosp_ed = 2021
)

current_years <- c(current_years, 
                   list(
                     currentYearG3 = paste0(current_years$currentYear-2, "-", current_years$currentYear), 
                     currentYearG5 = paste0(current_years$currentYear-4, "-", current_years$currentYear),
                     currentYearG3_hosp_ed = paste0(current_years$currentYear_hosp_ed-2, "-", current_years$currentYear_hosp_ed), 
                     currentYearG5_hosp_ed = paste0(current_years$currentYear_hosp_ed-4, "-", current_years$currentYear_hosp_ed),
                     minYear = 2000,
                     maxYear = current_years$currentYear
                   ))


# Set App Version ==============================================
VERSION <- "V2025.1"

# Set Data type (real or fake) ============================================
DATATYPE <- "fake"