trendGeneric <- function(myDemoGroup = "Sex",
                         myLHJ="CALIFORNIA",
                         myCause="0",
                         myMeasure = "YLL", 
                         mySex = "Total",
                         myYearGrouping=1, 
                         myLogTrans=FALSE, 
                         myMultiRace=FALSE) {
  
  # Testing ==================
  if(1==2){
    myDemoGroup = "Sex"
    myLHJ <- "CALIFORNIA" 
    myCause <-  "0"
    myMeasure <- "aRate"
    mySex <- "Total"
    myYearGrouping <- 1
    myLogTrans <- FALSE
    myMultiRace <- FALSE
  }
  
  # Setup =========================================================
  
  # Education Group - 1) If City, Stop 2) Set Min Year 3) Set myYearGrouping to 1
  
  # If City, stop
  if (myDemoGroup == "Education") {
    
    if (myLHJ %in% constants$cityLHJs) {
      stop(appText$cityMessage)
    }
    
    minYear <- 2012
    myYearGrouping <- 1
  }
  
  # Load death measures
  deathMeasures <- choices$deathMeasures_noSMR
  deathMeasuresNames <- names(deathMeasures)
  
  # 1. Set up Year Group Variable Names
  # 2. Load chartYearMap and modify
  # 3. Set plot breaks and labels
  if (myYearGrouping != 1) {
    
    year_var_dat <- ifelse(myYearGrouping == 3, "yearG3", "yearG5") # Death data
    year_var_map <- ifelse(myYearGrouping == 3, "yearGroup3", "yearGroup5") # chartYearMap
    midYear_var <- ifelse(myYearGrouping == 3, "midYear3", "midYear5") # chartYearMap
    
    chartYearMap <- lazy_load("myInfo/Year to Year-Group Linkage.xlsx") %>%
      select(-year) %>% 
      rename(yearG = !!as.symbol(year_var_map), year = !!as.symbol(midYear_var)) %>% 
      filter(!is.na(year)) %>% 
      select(yearG, year) %>% 
      unique()
    
    # Set myBreaks and myLabels
    myLabels   <- chartYearMap$yearG
    myBreaks   <- chartYearMap$year
    
  } else {
    
    year_var_dat <- "year"
    
    # Set myLabels and myBreaks
    myBreaks <- current_years$minYear:current_years$maxYear
    myLabels <- myBreaks
    
  }
  
  # Set demographic variable
  myDemoVariable <- case_when(myDemoGroup == "Sex" ~ "sex", 
                              myDemoGroup == "Age Group" ~ "ageGroup",
                              myDemoGroup == "Race/Ethnicity" ~ "raceNameShort",
                              myDemoGroup == "Education" ~ "eduNameShort"
  )
  
  # Demographic Group Text
  myDemoText <- case_when(myDemoGroup == "Sex" ~ "by Sex", 
                          myDemoGroup == "Age Group" ~ "by Age Group",
                          myDemoGroup == "Race/Ethnicity" ~ "by Race/Ethnicity",
                          myDemoGroup == "Education" ~ "by Educational Attainment"
  )
  
  # Plot Colors
  if (myDemoGroup == "Sex") {
    plotColors <- plot_colors$gender
  } else if (myDemoGroup == "Age Group") {
    plotColors <- plot_colors$age
  } else if (myDemoGroup == "Race/Ethnicity") {
    plotColors <- plot_colors$raceNameShort
  } else if (myDemoGroup == "Education") {
    plotColors <- plot_colors$eduNameShort
  }
  
  # Direct Labels Size
  myCex <- ifelse(myDemoGroup == "Education", 1.2, ggplot_standards$lineLabelSize)
  
  # Log Trans
  myTrans    <- ifelse(myLogTrans,'log2','identity')
  myMin      <- ifelse(myLogTrans,NA,0)   
  
  # Set Titles For Plot
  myYTitle <- deathMeasuresNames[deathMeasures == myMeasure]
  
  causeText <- links$deathCause %>% 
    filter(causeCode == myCause) %>% 
    pull(causeNameShort)
  
  eduText <- ifelse(myDemoGroup == "Education", 
                    paste0(", ", mySex, ", >24 years-old only, (crude age-adjustement)"), 
                    ""
                    )
  
  myTitle <- paste0("Trend in ",
                    myYTitle,
                    " of ", 
                    causeText,
                    " in ",
                    myLHJ, " ", 
                    myDemoText,
                    ", ",
                    myLabels[1],
                    " to ",
                    myLabels[length(myLabels)], 
                    eduText
                    )
  
  myTitle <-  str_wrap(myTitle, 70)
  
  # Load and process data =====================================================
  
  # Load data
  if (myDemoGroup == "Sex") {
    if (myYearGrouping == 1) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty.RDS"))
    } else if (myYearGrouping == 3) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty_3year.RDS"))
    } else if (myYearGrouping == 5) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty_5year.RDS"))
    }
  } else if (myDemoGroup == "Age Group") {
    if (myYearGrouping == 1) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datState_AGE.RDS")) %>% 
        filter(sex=="Total", !is.na(ageGroup))
    } else if (myYearGrouping == 3) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty_AGE_3year.RDS")) %>% 
        filter(sex=="Total", !is.na(ageGroup))
    }
  } else if (myDemoGroup == "Race/Ethnicity") {
    if (myYearGrouping == 1) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datState_RE.RDS")) %>% 
        filter(sex == "Total", !raceCode %in% c("Other", "Unknown", "Total")) %>% 
        left_join(select(links$race, raceCode, raceNameShort), by = "raceCode")
    } else if (myYearGrouping == 3) {
      inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty_RE.RDS")) %>% 
        filter(sex == "Total", !raceCode %in% c("Other", "Unknown", "Total")) %>% 
        left_join(select(links$race, raceCode, raceNameShort), by = "raceCode")
    }
  } else if (myDemoGroup == "Education") {
    eduLink <- links$edu
    inDat <- lazy_load(file.path("myData", DATATYPE, "datCounty_EDU.RDS")) %>% 
      filter(sex == mySex) %>% 
      left_join(select(eduLink, eduCode, eduNameShort), by = "eduCode")
  }
  
  
  # Process Data for plotting
  
  # If yearGroup, join chartYearMap
  if (myYearGrouping != 1) {
    inDat <- inDat %>%
      mutate(yearG = !!as.symbol(year_var_dat)) %>% 
      left_join(chartYearMap, by = "yearG") %>% 
      filter(!is.na(year))
  }
  
  # If Race and no multi-race, remove
  if (myDemoGroup == "Race/Ethnicity" && !myMultiRace) {
    inDat <- inDat %>% 
      filter(raceNameShort != "Multi-Race")
  }
  
  
  dat.1 <- inDat %>% 
    filter(county == myLHJ, causeCode == myCause) %>%
    left_join(select(links$deathCause,causeCode, causeName, causeNameShort), by= "causeCode")
  
  # If no data exists, stop function and return message
  if (nrow(dat.1)==0) stop(appText$stopMessage)
  

  # Process Data for downloading
  
  varsIn  <- unique(c("causeNameShort", "county", year_var_dat, "sex", myDemoVariable, myMeasure)) 
  
  if(myMeasure == "cDeathRate") varsIn <- c(varsIn, "rateSE", "rateLCI", "rateUCI")
  if(myMeasure == "aRate") varsIn <- c(varsIn, "aSE", "aLCI", "aUCI")
  
  tabDat  <- dat.1 %>% 
    select(all_of(varsIn)) %>% 
    arrange(!!as.symbol(year_var_dat))
  

  # Generic Plot =====================================================================================
  
  # REMOVES ALL INJURY CAUSES 2022 DATA POINTS SINCE MANY ARE CLASSIFIED AS Z01. COMMENT CODE BELOW WHEN READY TO SHOW 
  # if (substr(myCause, 1, 1) == "E") {
  #   if (myYearGrouping == 1) {
  #     dat.1 <- dat.1 %>% 
  #       filter(year != 2022)
  #     
  #     tabDat <- tabDat %>% 
  #       filter(year != 2022)
  #   }
  # }
  
  # X-Axis expansion
  left_expand <- ifelse(myDemoGroup == "Education", 3, 1)
  
  tplot <-  ggplot(data = dat.1, aes(x=year, y = get(myMeasure), color = get(myDemoVariable))) +
    geom_line(linewidth = ggplot_standards$lineSize, show.legend = FALSE) +
    geom_point(shape = ggplot_standards$pointShape, size = ggplot_standards$pointSize, show.legend = FALSE)  +
    scale_x_continuous(minor_breaks = myBreaks, 
                       breaks = myBreaks, 
                       labels = myLabels, 
                       expand = expansion(mult = c(0, 0), add = c(left_expand, 3))) +
    scale_y_continuous(trans = myTrans, limits = c(myMin, NA)) +
    scale_color_manual(values = plotColors) +
    geom_dl(aes(label = get(myDemoVariable)), method = list(dl.trans(x = x + 0.2), "last.points", cex = myCex, 'bumpup',font="bold")) +
    labs(y = myYTitle, x = "Year", title = myTitle) +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1))
  
  # Add direct labels to left if Education
  if (myDemoGroup == "Education") {
    tplot <- tplot +
      geom_dl(aes(label = get(myDemoVariable)), method = list(dl.trans(x = x - 0.2), "first.points", cex=myCex, 'bumpup', font="bold"))
  }
  
  # Return plot and data
  list(plotL = tplot, dataL = tabDat)
  
}


# Test 
if (F) {
  trendGeneric(myDemoGroup = "Age Group",
               myLHJ="CALIFORNIA",
               myCause="0",
               myMeasure = "cDeathRate", 
               myYearGrouping=1, 
               myLogTrans=FALSE, 
               myMultiRace=FALSE, 
               mySex = "Total"
               )
}