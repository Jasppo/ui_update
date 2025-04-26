# Constants ===================================================================

constants <- list(
  STATE = "CALIFORNIA",
  cityLHJs = c("Berkeley", "Long Beach", "Pasadena", "Alameda HD", "Los Angeles HD")
)

# Links ================================================================================

links <- function() {
  
  # Tab-Subtab
  tab_subtab_link <- read_excel("myInfo/ccbTabLink.xlsx")
  
  raceLink <-  read_excel("Standards/raceLink.xlsx")
  ageLink  <-  read_excel("Standards/ageLink.xlsx",sheet = "standard")
  eduLink <- read_csv("myInfo/Education Codes and Names.csv", show_col_types = F)
  
  countyLink <- read_excel("Standards/countyLink.xlsx")
  commInfo <- read.csv("myInfo/comName.csv", header = T)
  
  deathCauseLink <-read_excel("myInfo/icd10_to_CAUSE.xlsx", sheet="main") %>%
    filter(!is.na(causeList)) %>%
    mutate(causeNameShort = ifelse(is.na(causeNameShort), causeName, causeNameShort),
           topLevName     = case_when(topLevCode  == "0" ~ "All Causes",
                                      topLevCode  == "A" ~ "Communicable",
                                      topLevCode  == "B" ~ "Cancer",
                                      topLevCode  == "C" ~ "Cardiovascular",
                                      topLevCode  == "D" ~ "Other Chronic",
                                      topLevCode  == "E" ~ "Injury", 
                                      topLevCode  == "P" ~ "Perinatal",
                                      TRUE ~ "Ill-Defined")) %>%
    select(causeCode, causeName, causeNameShort, causeList, topLevCode, topLevName) %>%
    arrange(causeCode) %>%
    as.data.frame()
  
  hospCauseLink <- read_excel("myInfo/CCS Code and Names Linkage.xlsx") %>%
    mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
    mutate(causeNameShort = ifelse(is.na(ccsNameShort),ccsName,ccsNameShort)) %>%
    select(causeCode, causeName = ccsName, causeNameShort, topLevName, birth) %>%
    as.data.frame()
  
  return(list(race = raceLink, 
              age = ageLink, 
              edu = eduLink,
              county = countyLink,
              comm = commInfo, 
              deathCause = deathCauseLink, 
              hospCause = hospCauseLink, 
              ccbTab = tab_subtab_link
              ))
  
}

links <- links()

# App Text ===================================================================

appText <- function() {
  
  appText <- read_docx("myInfo/appText/appText.docx") # Read in appText Word Doc
  appText <- docx_extract_tbl(appText, 1) # Extract table
  appTextL  <- split(appText$Text, seq(nrow(appText))) # Convert data frame into a list
  names(appTextL) <- appText$varName # Add varNames to list
  
  # News and Updates
  updatesText <- read_docx("myInfo/appText/newsUseCCB_Word.docx") # Read in appText Word Doc
  updatesText <- docx_extract_tbl(updatesText, 1) # Extract table
  
  # Tab Information text
  tabInfo <- read_docx("myInfo/appText/tabInformation.docx") # Read in appText Word Doc
  tabInfo <- docx_extract_tbl(tabInfo, 1) # Extract table
  
  # Home Info section
  homeInfo <- read_docx("myInfo/appText/Homepage Information.docx")
  homeInfo <- docx_extract_tbl(homeInfo, 1)
  
  cityMessage <- "These data are not yet available for Local Health Department city jurisdictions of Berkeley, Long Beach, Pasadena and their corresponding county Local Health Departments of Alameda HD and Los Angeles HD."
  stopMessage <- "Sorry friend, data are suppressed per the California Health and Human Services Agency Data De-Identification Guidelines, or there are no cases that meet this criteria."
  supMessage <- "Note: All measures associated with counts < 11 as well as necessary complementrary counts/measures are excluded for data de-identification purposes"
  multiRaceNote <- "** Note: Multirace data are NOT RELIABLE due to changing data collection practices"
  
  return(list(
    app = appTextL, 
    updates = updatesText,
    tabInfo = tabInfo,
    cityMessage = cityMessage,
    stopMessage = stopMessage,
    supMessage = supMessage,
    multiRaceNote = multiRaceNote,
    homeInfo = homeInfo
  ))
  
}

appText <- appText()


# Colors ================================================================================================================================

# Plot Colors
plot_colors <- function() {
  
  paletteCB <- c("#0072B2", # darker blue, 
                 "#4A4A4A", # darker gray,
                 "#D55E00", # darker orange
                 "#117733", # green
                 "#56B4E9", # lightblue
                 "#4BE62F", # light green
                 "#E69F00", # lighter orange
                 "#CC79A7",  # pink
                 "#b22222", # firebrick
                 "cyan"
  )
  
  paletteCB_race <- paletteCB[c(
    2, # darker gray
    3, # darker orange
    5, # lightblue
    4, # green
    7, # lighter orange
    1, # darker blue
    9, # firebrick
    6, # light green
    8 # pink
  )]
  
  paletteCB_age <- c(paletteCB_race, paletteCB[10])
  
  # Top lev colors
  topLev              <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Perinatal","Other")
  topLevColors        <- paletteCB[1:length(topLev)]
  names(topLevColors) <- topLev
  
  # Toplev associated text colors (white or black) - for text in bars
  topLevTextColors <- c("#FFFFFF", "#000000","#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
  names(topLevTextColors) <- topLev
  
  raceLink <- links$race
  raceList <- raceLink[which(raceLink$raceCode %in% c("AIAN", "Asian", "Black", "Hisp", "Multi", "NHPI", "Other", "White", "Total")), ]
  
  
  # Race Names
  raceName <- raceList$raceName
  raceNameColors <- paletteCB_race[1:length(raceName)]
  names(raceNameColors) <- raceName
  
  # Race Name Short
  raceNameShort <- raceList$raceNameShort
  raceNameShortColors <- paletteCB_race[1:length(raceNameShort)]
  names(raceNameShortColors) <- raceNameShort
  
  # Gender Names
  genderNames         <- c("Female","Male", "Total")
  genderColors <- paletteCB[1:length(genderNames)]
  names(genderColors) <- genderNames
  
  # Age Names
  ageGroupColors <- paletteCB_age
  names(ageGroupColors) <- links$age$ageName
  
  # Edu Names
  eduLink <- links$edu
  eduNameShortColors <- paletteCB_race[1:length(eduLink$eduNameShort)]
  names(eduNameShortColors) <- eduLink$eduNameShort
  
  return(list(
    palette = paletteCB,
    topLev = topLevColors,
    topLevText = topLevTextColors,
    raceName = raceNameColors,
    raceNameShort = raceNameShortColors,
    gender = genderColors,
    age = ageGroupColors,
    eduNameShort = eduNameShortColors
  ))
  
}

plot_colors <- plot_colors()

# GGPlot Standards -----------------------


ggplot_standards <- function() {

  myWrapNumber <- 70
  line_color <- "#004c4c"
  bar_color <- "#00796B"

  myLineLabelSize <- 2

  myLineSize  <- 2
  myPointSize <- 5 # line markers
  myPointShape <- 18

  myTheme <- theme_bw() +
    theme(panel.background = element_rect(fill = "#e7e6e6"),
          panel.grid = element_line(color = "#f7f6f6"),
          strip.background = element_rect(fill = "#848884"),
          strip.text = element_text(size = 16, color = "white", face = "bold"),
          plot.title = element_text(size = 20, face = 'bold'),
          axis.title = element_text(size = 20, face="bold"),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20, face = "bold")
    )

  return(list(
    wrapNumber = myWrapNumber,
    lineColor = line_color,
    barColor = bar_color,
    lineLabelSize = myLineLabelSize,
    lineSize = myLineSize,
    pointSize = myPointSize,
    pointShape = myPointShape,
    theme = myTheme
  ))



}



# ggplot_standards <- function() {
#   
#   myWrapNumber <- 70
#   line_color <- "#004488"
#   bar_color <- "#004488"
#   
#   myLineLabelSize <- 2
#   
#   myLineSize  <- 2
#   myPointSize <- 5 # line markers
#   myPointShape <- 18
#   
#   myTheme <- theme_bw() +
#     theme(panel.background = element_rect(fill = "#e7e6e6"), 
#           panel.grid = element_line(color = "#f7f6f6"), 
#           strip.background = element_rect(fill = "#292562"),
#           strip.text = element_text(size = 16, color = "white", face = "bold"),
#           plot.title = element_text(size = 20, face = 'bold'),
#           axis.title = element_text(size = 20, face="bold"), 
#           axis.text.y = element_text(size = 16),
#           axis.text.x = element_text(size = 16),
#           legend.text = element_text(size = 20), 
#           legend.title = element_text(size = 20, face = "bold")
#     )
#   
#   return(list(
#     wrapNumber = myWrapNumber,
#     lineColor = line_color,
#     barColor = bar_color,
#     lineLabelSize = myLineLabelSize,
#     lineSize = myLineSize,
#     pointSize = myPointSize,
#     pointShape = myPointShape,
#     theme = myTheme
#   ))
#   
#   
#   
# }

ggplot_standards <- ggplot_standards()
theme_set(ggplot_standards$theme)

# App Theme Colors ============================================================

# CDPH logo colors:
# teal: #007b8b
# light blue: #0077cf
# dark blue: #00269b
# dark orange: #bc4800
# purple: #9d1e96
# light orange: #c06b12

# Primary: 
# A color to be used for hyperlinks, to indicate primary/default actions, and to show active selection state in some Bootstrap components. 
# Generally a bold, saturated color that contrasts with the theme's base colors.

# Secondary: 
# A color for components and messages that don't need to stand out. (Not supported in Bootstrap 3.)

# Success: 
# A color for messages that indicate an operation has succeeded. Typically green.

# Info:
# A color for messages that are informative but not critical. Typically a shade of blue-green.

# Warning
# A color for warning messages. Typically yellow.

# Danger:
# A color for errors. Typically red.


app_colors <- list(
  bg = "white",
  fg = "#333333",
  primary = "#004c4c",
  secondary = "#bc4800",
  success = "#007b8b",
  info = "#007b8b",
  warning = "#bc4800",
  danger = "red" 
)
