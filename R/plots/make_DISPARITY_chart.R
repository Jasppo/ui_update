disparity <- function(myLHJ="CALIFORNIA",
                      myCause="A", 
                      myYearGrouping=3, 
                      myCompare="Lowest Rate") {
  
  # Testing
  if(1==2){
    myLHJ = "CALIFORNIA" 
    myCause = "0"
    myYearGrouping <- 3
    myCompare <- "Highest Rate"
  }
  
  
  # Define Colors
  lowColor  <- "green"
  midColor  <- "blue"
  highColor <- "red"
  
  # 1 Year (Statewide) or 3-Year (County + Statewide)
  tYearGrp3 <- ifelse(myLHJ == constants$STATE & myYearGrouping == 1, as.character(current_years$maxYear), current_years$currentYearG3)
  
  # Colors and legend
  if (myCompare == "Highest Rate") {
    
    # Add a static caption with boxes
    caption <- paste0(
      "<b>Color Legend:</b> ",
      "<span style='color:red'>&#9607; Highest</span> ",
      "<span style='color:green'>&#9607; Sig. Lower (p<.01)</span> ",
      "<span style='color:blue'>&#9607; No Difference</span>"
    )
    
    # To join with plot data frames
    colorDF <- data.frame(pMark = c("Highest", "Sig. Lower (p<.01)", "No Difference"), 
                          color = c(highColor, lowColor, midColor)
    )
    
  } else {
    
    # Add a static caption with boxes
    caption <- paste0(
      "<b>Color Legend:</b> ",
      "<span style='color:green'>&#9607; Lowest</span> ",
      "<span style='color:red'>&#9607; Sig. Higher (p<.01)</span> ",
      "<span style='color:blue'>&#9607; No Difference</span>"
    )
    
    
    colorDF <- data.frame(pMark = c("Lowest", "Sig. Higher (p<.01)", "No Difference"), 
                          color = c(lowColor, highColor, midColor)
    )
                          
  }
  
  
  
  # RACE ------------------------------------------------------------------------------------------------------------------------
  
  if(myCompare == "Highest Rate") {
    
    raceTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_raceHigh.RDS")) %>% 
      filter(raceCode != "Multi")
    
  } else {
    
    raceTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_raceLow.RDS")) %>% 
      filter(raceCode != "Multi")
    
  }
  
  
  plot_df_race <- filter(raceTest, county == myLHJ,causeCode == myCause, yearG3==current_years$currentYearG3, sex == "Total") %>% 
    left_join(select(links$race, raceCode, raceName, raceNameShort),by="raceCode") %>% 
    left_join(colorDF, by = "pMark") %>% 
    mutate(measure = aRate, strata = raceNameShort) %>% 
    arrange(raceNameShort)
  
  if (nrow(plot_df_race)==0) stop(appText$stopMessage)
  
  raceDF <- plot_df_race %>%
    mutate(ageGroup = "Total", rateType = "Age-Adjusted Death Rate") %>%
    left_join(select(links$deathCause, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = aRate, rateSE = aSE, LCI, UCI, compareGroup = lowRace, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  
  
  
  # AGE ------------------------------------------------------------------------------------------------------------------------
  
  if(myCompare == "Highest Rate") {
    ageTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_ageHigh.RDS"))
  } else {
    ageTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_ageLow.RDS"))
  }

  
  plot_df_age <- filter(ageTest,county == myLHJ,causeCode == myCause, yearG3==current_years$currentYearG3, sex == "Total") %>%
    mutate(ageGroup = factor(ageGroup,levels= links$age$ageName)) %>% 
    left_join(colorDF, by = "pMark") %>%
    mutate(measure = cDeathRate, strata = ageGroup) %>%
    arrange(ageGroup)
  
  if (nrow(plot_df_age)==0) stop(appText$stopMessage)
  
  
  ageDF <- plot_df_age %>%
    mutate(raceName = "Total", rateType = "Age-Specific Death Rate") %>%
    left_join(select(links$deathCause, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = cDeathRate, rateSE, LCI, UCI, compareGroup = lowAge, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  
  

  #SEX ------------------------------------------------------------------------------------------------------------------------
  
  if(myCompare == "Highest Rate") {
    sexTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_sexHigh.RDS"))
  } else {
    sexTest <- lazy_load(file.path("myData/", DATATYPE, "disparity_sexLow.RDS"))
  }
  
  plot_df_sex <- filter(sexTest,county == myLHJ,causeCode == myCause, yearG3==current_years$currentYearG3) %>%
    left_join(colorDF, by = "pMark") %>%
    mutate(measure = aRate, strata = sex) %>%
    arrange(sex)
  
  if (nrow(plot_df_sex)==0) stop(appText$stopMessage)
  
  
  sexDF <- plot_df_sex %>%
    mutate(ageGroup = "Total", raceName = "Total", rateType = "Age-Adjusted Death Rate") %>%
    left_join(select(links$deathCause, causeCode, causeName), by = "causeCode") %>%
    select(yearG3, county, causeName, sex, ageGroup, raceName, Ndeaths, rateType, 
           rate = aRate, rateSE = aSE, LCI, UCI, compareGroup = lowRace, compareRate = bestRate, 
           compareSE = bestSE, rateRatio, Ztest, pValue, pMark)
  
  
  
  # Plot ---------------------------------------------------------------------------------------------
  
  create_plot <- function(myData, myX_title, myY_title) {
    
    hchart(myData, "column", hcaes(x = strata, y = measure)) %>%
      hc_add_series(myData, "errorbar", 
                    hcaes(x = strata, y = measure, low = LCI, high = UCI)
      ) %>% 
      hc_plotOptions(
        column = list(colorByPoint = TRUE, 
                      colors = myData$color), 
        errorbar = list(color = "black", 
                        stemWidth = 1)
      ) %>% 
      hc_xAxis(title = list(text = myX_title)) %>% 
      hc_yAxis(title = list(text = myY_title), 
               gridLineWidth = 1, 
               gridLineColor = "#f7f6f6"
      ) %>% 
      hc_chart(plotBackgroundColor = "#EBEBEB") %>% 
      hc_subtitle(text = caption, align = "center")  # Add the caption as a subtitle
    
  }
  
  racePlot <- create_plot(plot_df_race, 
                          "Race/Ethnicity", 
                          "Age-Adjusted Death Rate"
                          )
  
  sexPlot <- create_plot(plot_df_sex, 
                         "Sex", 
                         "Age-Adjusted Death Rate"
  )
  
  
  agePlot <- create_plot(plot_df_age, 
                          "Age Group", 
                          "Crude Death Rate"
  )
  
  
  # Download data ---------------------------------------------------------------------------
  
  df <- bind_rows(sexDF, raceDF, ageDF)
  
  list(plotL_sex = sexPlot, 
       plotL_race = racePlot, 
       plotL_age = agePlot, 
       dataL = df)
  
}


# Test
if (F) {
  disparity(myLHJ="CALIFORNIA",
            myCause="A", 
            myYearGrouping=3, 
            myCompare="Lowest Rate")
}