STATE       <- "CALIFORNIA"

currentYear          <- 2021
currentYear_hosp_ed  <- 2021

incRecentYearData <- T 
currentYear <- ifelse(incRecentYearData, currentYear, currentYear - 1)

incRecent_MultiYearData <- T   

t.year3 <- currentYear - 2
t.year5 <- currentYear - 4

yearGrp3 <- paste0(t.year3, "-", currentYear)
yearGrp5 <- paste0(t.year5, "-", currentYear)

minYear      <- 2000
maxYear      <- currentYear


# For Hosp_ED_Deaths  
# use this FOR ALL combine  hosp/ed/Death work
# AND maybe add incRecent year type switch
yearGrp3_hosp_ed     <- paste0(currentYear_hosp_ed-2, "-", currentYear_hosp_ed)
yearGrp3_hosp_ed_num <- (currentYear_hosp_ed-2):(currentYear_hosp_ed)

yF           <- 100000  # rate constant 


# NOTE: "LABEL" was changed to "causeCode" 12/2020
# changed "BG" to "topLev"

# ccs_code_to_name_link.csv  is going away, 
# incudes ccs_diagP	and ccsName variables
# should be replaced  with files below and ccsCode and ccsName  OR whatever we call thouse

raceLink    <-  read_excel("Standards/raceLink.xlsx")
ageLink     <-  read_excel("Standards/ageLink.xlsx",sheet = "standard")


# Sex-specific cancers ============================================

# Breast cancer (F), Uterine Cancer
cancer_female <- c("Breast cancer", "Uterine cancer", "Ovary cancer")
cancer_male <- c("Prostate cancer")


## COLORS==========================================================


# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

# The palette with grey:
# greyPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Color-blind friendly (9 colors) palette 
paletteCB <- c("#0072B2", # darker blue, 
               "#4A4A4A", # darker gray,
               "#D55E00", # darker orange
               "#117733", # green
               "#56B4E9", # lightblue
               "#4BE62F", # light green
               "#E69F00", # lighter orange
               "#CC79A7",  # pink
               "#b22222" # firebrick
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

# Top lev colors
topLev              <- c("Communicable","Cancer","Cardiovascular","Other Chronic","Injury","Ill-Defined","Perinatal","Other")
# topLevColors        <- brewer.pal(n = length(topLev), name = "Dark2") # Determine pallete; Dark2 allows max 8
topLevColors        <- paletteCB[1:length(topLev)]
names(topLevColors) <- topLev

# Toplev associated text colors (white or black) - for text in bars
topLevTextColors <- c("#FFFFFF", "#000000","#000000", "#000000", "#000000", "#000000", "#000000", "#000000")
names(topLevTextColors) <- topLev




#FROM plot life tables function

# raceColors1        <- c("seashell4", "chocolate1", "firebrick", "royalBlue1", "darkblue", "navajowhite3", "red",   "chartreuse4")
# raceNames1         <- c("AIAN_NH",   "ASIAN_NH",   "BLACK_NH",  "HISPANIC",   "MR_NH",    "NHPI_NH",      "TOTAL", "WHITE_NH")
# names(raceColors1) <- raceNames1
# totalColor         <- "red"
# 
# raceNames         <- raceLink$raceName 
# raceColors        <- brewer.pal(length(raceNames), "Paired")
# names(raceColors) <- raceNames

raceList <- raceLink[-8:-10,"raceNameShort"] %>% unlist() %>% unname()

# Race Codes
#raceCodes <- c("AIAN", "Asian", "Black", "Hisp", "Multi", "NHPI", "Other", "White", "Total")
#raceCodesColors <- c(greyPalette[1:(length(raceCodes) - 1)], "red")
# raceCodesColors <- c(greyPalette[1:length(raceCodes)], "red")
# raceCodesColors <- c("red", "yellow", "black", "brown", "blue", "lightblue", "darkblue", "gray","purple")

#names(raceCodesColors) <- raceCodes
#totalColor         <- "red"

# Race Names
raceName <- c("American Indian or Alaska Native", 
              "Asian", 
              "Black",
              "Latino",
              "Multi-Race",
              "Native Hawaiian and other Pacific Islander", 
              "Other",
              "White", 
              "Total")
raceNameColors <- paletteCB_race[1:length(raceName)]
names(raceNameColors) <- raceName

# Race Name Short
raceNameShort <- c("AI/AN", 
                   "Asian", 
                   "Black",
                   "Latino",
                   "Multi-Race",
                   "NH/PI", 
                   "Other",
                   "White", 
                   "Total")
raceNameShortColors <- paletteCB_race[1:length(raceNameShort)]
names(raceNameShortColors) <- raceNameShort



genderNames         <- c("Female","Male", "Total")
# genderColors          <- c("gray","lightblue")
genderColors <- paletteCB[1:length(genderNames)]
names(genderColors) <- genderNames



#===========================================================================


commInfo <- read.csv("myInfo/comName.csv", header = T)


deathCauseLink   <-
  read_excel("myInfo/icd10_to_CAUSE.xlsx", sheet="main")%>%
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


hospCauseLink  <-    read_excel("myInfo/CCS Code and Names Linkage.xlsx") %>%
  mutate(causeCode = str_pad(ccsCode, 5,"left",pad="o")) %>%
  mutate(causeNameShort = ifelse(is.na(ccsNameShort),ccsName,ccsNameShort)) %>%
  select(causeCode, causeName = ccsName, causeNameShort, topLevName, birth) %>%
  as.data.frame()


# ggplot standards ---------------------

myTitleSize <- 20
myLegendSize <- 20

myTextSize <- 18
myAxisTextSize <- myTextSize2 <- 16
myAxisSize  <- myAxisTitleSize <- myTextSize3 <- 20

myWrapNumber <- 70
myTitleColor <- "darkblue"
line_color <- "#004488"
bar_color <- "#004488"

myCex1           <- 2  # 1.5  #line labels
myCex2           <- 1.2  #currently used only in education trend
myLineLabelSpace <- 0.3

myLineLabelCex <- 2

myLineSize  <- 2
myPointSize <- 5 # line markers
myPointShape <- 18

myTheme <- theme_bw() +
  theme(plot.title   = element_text(size = myTitleSize, color=myTitleColor, face = 'bold'),
        strip.text.y = element_text(size = myTextSize2, face="bold", angle = 0),
        strip.text.x = element_text(size = myTextSize2, face="bold", angle = 0),
        axis.title   = element_text(size = myAxisTitleSize, face="bold"), # was myTextSize2, changed to myAxisSize
        axis.text.y  = element_text(size = myAxisTextSize),
        axis.text.x  = element_text(size = myAxisTextSize), 
        legend.text = element_text(size = myLegendSize), 
        legend.title = element_text(size = myLegendSize)
        #axis.text.x  = element_text(size = 10,          face="bold", angle = 40, hjust = 1),
  )

# New theme
myTheme <- theme_bw() +
  theme(panel.background = element_rect(fill = "#e7e6e6"), 
        panel.grid = element_line(color = "#f7f6f6"), 
        strip.background = element_rect(fill = "#292562"),
        strip.text = element_text(size = myTextSize2, color = "white", face = "bold"),
        plot.title   = element_text(size = myTitleSize, face = 'bold'),
        axis.title   = element_text(size = myAxisTitleSize, face="bold"), 
        axis.text.y  = element_text(size = myAxisTextSize),
        axis.text.x  = element_text(size = myAxisTextSize),
        legend.text = element_text(size = myLegendSize), 
        legend.title = element_text(size = myLegendSize, face = "bold")
  )

theme_set(myTheme)

cdph1 <- 
  theme_bw() + 
  theme(panel.grid.minor = element_blank(), 
        plot.title       = element_text(size = 16, color="darkblue"),
        axis.title       = element_text(size = 14), 
        axis.text  = element_text(size = 14)
  ) 


# color standards -------------------------------

# ADA Compliant colors - http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette