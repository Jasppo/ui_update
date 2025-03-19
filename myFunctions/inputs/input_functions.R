# ===================================================================================
# Functions for Showing & Hiding sidebar Inputs.  (actual inputs are in input_widgets.R)
# Jonah Golden, October 8 2019
# ===================================================================================


# Constants =========================================================================

# List of all show/hide inputs

r_global_inputs <- reactiveValues(
  myTabInformation = T,
  myLHJ = constants$STATE, 
  myCAUSE = "0",
  myMeasure = "aRate",
  mySex = "Total",
  suppressionNote = T,
  myDataDownload = T,
  myChartDownload = T
)

INPUTS <- isolate(names(r_global_inputs))


TAB_INPUTS <- list(homeTab=c(),
                   mapsTab = c("myTabInformation", "myCAUSE", "myLHJ", "myMeasure", "mySex", "suppressionNote", "myDataDownload", "myChartDownload"),
                   rankByCauseTab = c("myTabInformation", "myLHJ", "mySex", "suppressionNote", "myDataDownload", "myChartDownload"),
                   rankByGeoTab = c("myTabInformation", "myCAUSE", "myLHJ", "mySex", "myMeasure", "suppressionNote", "myDataDownload", "myChartDownload"),
                   demoTrendTab = c("myTabInformation", "myCAUSE", "myLHJ", "suppressionNote", "myDataDownload", "myChartDownload"),
                   lifeExpTab = c("myTabInformation", "myLHJ", "myDataDownload", "myChartDownload"),
                   topTrendsTab =c ("myTabInformation", "myLHJ", "myMeasure", "suppressionNote", "myDataDownload", "myChartDownload"),
                   disparitiesTab = c("myTabInformation", "myCAUSE", "myLHJ", "suppressionNote", "myDataDownload", "myChartDownload"),
                   demographicsTab = c("myTabInformation", "myLHJ", "myDataDownload", "myChartDownload")
)

# Functions =========================================================================

# Self Explanatory
hideAllInputs <- function() {
  for (input in INPUTS) { shinyjs::hide(input) }
}

# Main function for updating input widgets ------------------------------------------

updateInputsOnTabId <- function(tabID) {
  
  hideAllInputs()
  
  # 1. Build list of desired inputs
  curr_tab_inputs = get(tabID, TAB_INPUTS)
  
  # 2. Show desired inputs, hide rest. (smoother transition in the UI if you show THEN hide):
  for (input in curr_tab_inputs) { shinyjs::show(input) }  
  #for (input in setdiff(INPUTS, curr_tab_inputs)) { hide(input) } "opposite approach"
  
}

# Download data and chart
download <- function(myID, myData, myChart, myLHJ, myWidth = 18, myHeight = 10, myPointSize = 10, myRes = 100) {
  
  data <- downloadHandler(filename = function() { paste0(myID, "-", myLHJ, "-", Sys.Date(), ".csv") },
                          content = function(file) {
                            write.csv(myData, file, row.names = FALSE)
                          })
  
  chart <- downloadHandler(filename = function() { paste0(myID, "-", myLHJ, "-", Sys.Date(), ".png") },
                           content = function(file) {
                             png(file, width = myWidth, height = myHeight, units = "in", pointsize = myPointSize, res = myRes)
                             print(myChart)
                             dev.off()
                           })
  
  return(list(dataL = data, plotL = chart))
  
  
}