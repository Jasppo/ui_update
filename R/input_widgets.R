# Help Styles for Inputs =============================================

inputStyles <- list(
  myInputHelpButtonSty = "width:20px;color:#fff; background-color:#e17b61; border-color:#e17b61; padding:0px; font-size: 18px;margin:0px;margin-left:10px;float:right;",
  helpIcon = "?", 
  myInputHelpButtonSty_broadGroup = "width:20px;  color:#fff; background-color:#e17b61; border-color:#e17b61; padding:0px; font-size: 18px;margin:0px;margin-left:10px;display:inline-block;"
)

# Input choices ==============================================================

choices <- function() {
  
  # LHJs
  lhjs <- sort(c(constants$STATE, links$county$countyName))
  
  # Cause - Full List, Public Health Level List, Top Level List
  fullList          <- links$deathCause$causeCode
  names(fullList)   <- links$deathCause$causeList
  
  phList            <- links$deathCause %>% filter(nchar(causeCode) <= 3)
  phCode            <- phList$causeCode
  names(phCode)     <- phList$causeList
  
  bigList           <- links$deathCause %>% filter(nchar(causeCode) <= 1)
  bigCode           <- bigList$causeCode
  names(bigCode)    <- bigList$causeList
  
  
  # Death Measures
  deathMeasures <- c("Ndeaths", "cDeathRate", "aRate", "YLL", "YLLper", "YLL.adj.rate", "mean.age", "SMR")
  names(deathMeasures) <- c("Number of deaths", "Crude Death Rate per 100,000 population", "Age-Adjusted Death Rate",
                            "Years of Life Lost (YLL)", "YLL Rate per 100,000 population", "Age-Adjusted YLL Rate",
                            "Mean Age at Death", "Standard Mortality Ratio")
  
  deathMeasures_noSMR  <- deathMeasures[deathMeasures != 'SMR'] # MOST TABS
  deathMeasures_ageTrend <- deathMeasures_noSMR[!deathMeasures_noSMR %in% c("aRate", "YLL.adj.rate")] # Age trends
  deathMeasures_sort <- deathMeasures[deathMeasures %in% c("Ndeaths", "YLLper", "aRate", "mean.age", "SMR")] # Rank By Cause Tab
  
  
  return(list(
    lhjs = lhjs,
    cause_fullList = fullList,
    cause_phList = phCode,
    cause_broadList = bigCode,
    deathMeasuresAll = deathMeasures,
    deathMeasures_noSMR = deathMeasures_noSMR,
    deathMeasures_ageTrend = deathMeasures_ageTrend,
    deathMeasures_sort = deathMeasures_sort
  ))
  
}

choices <- choices()



# Input Widgets ================================================

## Cause of Death Dropdown ======================

dottedSelectInput <- function(inputId, label, choices, height = "500px") {
  list(
    tags$style(HTML(paste0(
      sep = "\n",
      # Jaspreet: this only changes the myCAUSE dropdown 
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y { ",
      "width: auto !important;",
      "min-width: 25% !important;", # set to 25% because this is now a child of the body
      " overflow: auto;", # Jaspreet: provides a scrollbar to dropdown menus if needed
      "white-space: nowrap;",
      "height: 350px;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown > ul {",
      "  margin: 0 !important;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown [data-selectable] {",
      "  padding: 5px 15px !important;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content {",
      "  height: 100% !important;",
      "  max-height: 100% !important",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul {",
      "  padding-left: 15px;",
      "  margin-bottom: 0;",
      "  list-style: none;",
      "  line-height: 18px;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul {",                   # Level 0 font size, color, weight
      "  font-size: 18px;",                              
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul {",              # Level 1 font size, color, weight
      "  font-size: 16px;",
      "  font-weight: bold;",
      "  color: rgb(23,78,134);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul > ul {",          # Level 2 font size, color, weight 
      "  font-size: 14px;",
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul > ul > ul {",     # Level 3 font size, color, weight  
      "  font-size: 14px;",
      "  font-weight: normal;",
      "  color: rgb(23,78,134);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > li {",
      "  font-size: inherit;",
      "  color: inherit;",
      "}"
    ))),
    selectizeInput(
      inputId = inputId,
      label = label,
      choices = choices,
      options = list(
        maxOptions = length(choices),
        dropdownParent = 'body', # Jaspreet - Avoid the clipping issue
        render = I(
          paste(
            collapse = " ", sep = " ",
            "{ option: function(item, escape) { ",
            "if (item.label.substring(0, 1) !== '.') {",
            "return '<div>' + item.label + '</div>';",
            "}",
            "var dots = item.label.match(/^[.]+/)[0];",
            "var text = item.label.replace(/^[.]+/, '');",
            "var open = dots.replace(/[.]/g, '<ul>');",
            "var close = dots.replace(/[.]/g, '</ul>');",
            "return open + '<li><div>' + text + '</div></li>' + close;",
            "}, ",
            "item: function(item, escape) { ",
            "return '<div>' + item.label.replace(/^[.]+/, '') + '</div>';",
            "}" ,
            "}"
          )
        )
      ), 
      tags$head(tags$style(".selectize-control.single { width: 400px; z-index: 1; }"))
    )
  )
}


## List of  All input widgets ==============================

# Notes: 
# - Order of widgets matter here. All widgets placed on sidebar upon app startup, but initially hidden.
# - Name of each element needs to be set to corresponding inputId.

input_widgets <- list(
  
  # Tab Information:
  myTabInformation = actionButton(inputId = "myTabInformation", label = "Show Tab Information"),
  
  # Link to Current View:
  copyLink = actionButton("copyLink", "Copy Link to This View", icon = icon("link")),
  
  # CCB Death Condition List:
  myCAUSE = dottedSelectInput("myCAUSE", 
                              label=list(strong("Cause of Death:"), actionButton(inputId="causeHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)), 
                              choices=choices$cause_fullList),
  
  # LHJ/State:
  myLHJ = selectInput("myLHJ", strong("LHJ/State:"), choices = choices$lhjs, selected = constants$STATE),
  
  # Geography Level:
  myGeo = selectInput("myGeo", strong("Geographic Level:"), choices=c("County","Community","Census Tract")),
  
  # Geography Level Note:
  myGeoHelpText = div(id = "myGeoHelpText", 
                      helpText(h6(appText$app$tractWarning,style="color:red; float:left; margin: 20px;"))),
  
  myDemoGroup = selectInput(inputId = "myDemoGroup", label = strong("Display Trends By:"), 
                            choices = c("Sex", "Age Group", "Race/Ethnicity", "Education"), selected = "Sex"),
  
  # Death Measures:
  myMeasure = selectInput("myMeasure",
                          label=list(strong("Measure:"), actionButton("measureHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                          choices = choices$deathMeasuresAll, selected = "aRate"),
  
  # Death Measures - Sort By:
  myMeasure_sort = selectInput("myMeasure_sort",  label=list(strong("Measure Sort Order:"), actionButton("measureHelp", label=inputStyles$helpIcon,style=inputStyles$myInputHelpButtonSty)),
                               choices = choices$deathMeasures_sort, selected = "aRate"),
  
  # Sort Mean Age at Death by:
  myMeanAge_sort = radioButtons("myMeanAge_sort",  strong('Sort "Mean Age at Death" from:'),  
                                choices = c("Youngest to Oldest", "Oldest to Youngest")),
  
  # Cause Level:
  myLev = radioButtons("myLev", label=list(strong("Levels to show:"), actionButton("levelHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                       choices=c("Top" = "lev1","Public Health" = "lev2","Detail" = "lev3"), inline=TRUE, selected = 'lev2'),
  
  # Year Range:
  myYear = sliderInput("myYear", strong("Year:"), value=current_years$maxYear, min=2000, max=current_years$maxYear,
                       animate = FALSE, round=TRUE, sep="", step=1),
  
  # Year Grouping:
  myYearGrouping = radioButtons(inputId = "myYearGrouping", label = strong("Years to Group:"), choices=c(1, 3, 5), inline = TRUE, selected = 1),
  
  # Sex:
  mySex = radioButtons("mySex", label=strong("Sex:"), choices = c("Total", "Female", "Male"), inline=TRUE),
  
  # Top N:
  myN = numericInput("myN", strong("How Many:"), value=10,min=1,max= 50),
  
  # Stat Sig - Comparison Group:
  myCompare = radioButtons("myCompare", 
                           list(label = strong("Compare to group with:"), actionButton("disparityCompareHelp", label = inputStyles$helpIcon, style = inputStyles$myInputHelpButtonSty)), 
                           choices=c("Lowest Rate","Highest Rate")),
  
  # State based cutpoints:
  myStateCut = checkboxInput("myStateCut", 
                             label=list(strong("State-based cutpoints"), actionButton("stateCutHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                             value=TRUE),
  
  # Cut point method
  myCutSystem = radioButtons("myCutSystem", 
                             label = list(strong("Cut-point method:"), actionButton("cutmethodHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)), 
                             choices = c("Quantile", "Fisher"), inline = TRUE),
  
  # Log Transform:
  myLogTrans = checkboxInput(inputId = "myLogTrans", label = strong("Log Transform of Y Axis"), value=FALSE),
  
  # Include Multi-Race Line:
  myMultiRace = checkboxInput(inputId = "myMultiRace", label = strong("Include Multirace Line"), value=FALSE),
  
  # Multi-Race Note:
  myMultiRaceNote = div(id = "myMultiRaceNote", helpText(h6(appText$multiRaceNote, style="color:red; float:left; margin: 20px;"))),
  
  # Confidence Interval:
  myCI = checkboxInput("myCI", strong("95% CIs"), value=FALSE),
  
  # Reference Line:
  myRefLine = checkboxInput("myRefLine",  strong("Reference Line"), value=FALSE),
  
  # Population Data - Demo Group:
  myPopData_demoGroup = selectInput(inputId = "myPopData_demoGroup", label = strong("In the bottom right chart, display trends by:"), 
                                    choices = c("Total", "Sex", "Race/Ethnicity", "Age Group"), selected = "Total"),
  
  # Suppression Note:
  suppressionNote = div(id="suppressionNote",
                        appText$supMessage, style="color:blue;font-size:12px;padding-left:5px;"
                        ), 
  
  # Download Buttons
  myDataDownload = downloadButton(outputId = "myDataDownload", label = "Data"),
  myChartDownload = downloadButton(outputId = "myChartDownload", label = "Chart")
  
)

# Define which inputs are in each tab =======================================================================

# Note:
# - "myGeoHelpText": In mapsTab, but not in vector below since only shown when "myGeo" equals Census Tract
# - "myMultiRaceNote": In demoTrendTab, but not in vector below since only shown when "myDemoGroup" = "Race/Ethnicity" and "myMultiRace" is checked

TAB_INPUTS <- list(homeTab=c(),
                   mapsTab = c("myTabInformation", "copyLink", "myCAUSE", "myLHJ", "myGeo", "myMeasure", "myYear", "mySex", "myStateCut", "myCutSystem", "suppressionNote", "myDataDownload"),
                   rankByCauseTab = c("myTabInformation", "copyLink", "myLHJ", "myMeasure_sort", "myMeanAge_sort", "myYear", "mySex", "myN", "myLev", "suppressionNote", "myDataDownload", "myChartDownload"),
                   rankByGeoTab = c("myTabInformation", "copyLink", "myCAUSE", "myLHJ", "myMeasure", "mySex", "myYear", "myRefLine", "myCI", "suppressionNote", "myDataDownload", "myChartDownload"),
                   demoTrendTab = c("myTabInformation", "copyLink", "myCAUSE", "myLHJ", "myDemoGroup", "myMeasure", "myYearGrouping", "myLogTrans", "myMultiRace", "suppressionNote", "myDataDownload", "myChartDownload"),
                   lifeExpTab = c("myTabInformation", "copyLink", "myLHJ"),
                   topTrendsTab =c ("myTabInformation", "copyLink", "myLHJ", "suppressionNote"),
                   disparitiesTab = c("myTabInformation", "copyLink", "myCAUSE", "myLHJ", "myYearGrouping", "myCompare", "suppressionNote", "myDataDownload"),
                   demographicsTab = c("myTabInformation", "copyLink", "myLHJ", "myYear", "myPopData_demoGroup")
)


# Functions for showing/hiding inputs based on tab =============================================

hideAllInputs <- function() {
  for (input_id in names(input_widgets)) {
    shinyjs::hide(input_id)
  }
}

showTabInputs <- function(tabID) {
  hideAllInputs()
  
  # Get list of input IDs for this tab
  tab_inputs <- TAB_INPUTS[[tabID]]
  
  # Show those inputs
  for (input_id in tab_inputs) {
    shinyjs::show(input_id)
  }
}


# Function for downloading data and charts =====================================================

# Download data and chart
download <- function(myID, output, myData = NULL, myChart = NULL, 
                     myWidth = 18, myHeight = 10, myPointSize = 10, myRes = 100) {
  
  if (!is.null(myData)) {
    output$myDataDownload <- downloadHandler(
      filename = function() paste0("CCB-", myID, "-", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(myData(), file, row.names = FALSE)
      }
    )
  }
  
  if (!is.null(myChart)) {
    output$myChartDownload <- downloadHandler(
      filename = function() paste0("CCB-", myID, "-", Sys.Date(), ".png"),
      content = function(file) {
        png(file, width = myWidth, height = myHeight, units = "in", pointsize = myPointSize, res = myRes)
        print(myChart())
        dev.off()
      }
    )
  }
  
}
