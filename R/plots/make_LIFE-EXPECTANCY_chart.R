LEtrend <- function(myLHJ="CALIFORNIA", 
                    mySexMult, 
                    myRace, 
                    myCI, 
                    myYearGrouping = 1) {
  
  
  # Tesing ===============================
  
  if(1==2){
    
    myLHJ="Alameda" 
    mySexMult = c("Male","Female")
    myRace <- "Total"
    myCI = FALSE
    myYearGrouping = 1
    
  }  
  
  # Load and process data ================================
  
  if (myLHJ == constants$STATE) {
    
    lifeData <- lazy_load("myData/life_expectancy/e0ciState.RDS") %>% 
      mutate(county = constants$STATE)
    
  } else {
    
    countyLink <- links$county %>%  
      select(GEOID = FIPSCounty, county = countyName)
    
    lifeData <- lazy_load("myData/life_expectancy/e0ciCounty.RDS") %>% 
      mutate(GEOID = substr(GEOID, 3, 5)) %>% 
      left_join(countyLink, by = "GEOID")
  }
  
  tDat <- lifeData %>% 
    left_join(select(links$race, raceCode, raceNameShort), by = "raceCode") %>% 
    filter(county==myLHJ, sex %in% mySexMult, raceNameShort %in% myRace, nyrs == myYearGrouping) %>% 
    mutate(lineLabel = case_when(sex == "Total" & raceNameShort == "Total" ~ "Total", 
                                 sex != "Total" & raceNameShort == "Total" ~ sex,
                                 sex == "Total" & raceNameShort != "Total" ~ raceNameShort,
                                 sex != "Total" & raceNameShort != "Total" ~ paste(raceNameShort,"-",sex),
                                 ), 
           lineLabel = ifelse(lineLabel == "Latino - Female", "Latina - Female", lineLabel)
           )
  
  if (nrow(tDat)==0) stop(appText$stopMessage)
  
  # Plot =============================================================
  
  # Set min year and max year
  minYear_LT <- min(tDat$year)
  maxYear_LT <- max(tDat$year)
  
  # Set plot breaks and labels
  myBreaks <- minYear_LT:maxYear_LT
  myLabels <- myBreaks
  
  # Set plot title
  myTitle <- paste0("Trend in Life Expectancy, ",myLHJ,", ",minYear_LT,"-",maxYear_LT)
  myTitle <-  str_wrap(myTitle, 80)
  
  tplot<- ggplot(data = tDat, aes(x = year, y = ex)) +  
    geom_line(linewidth = 1.6, aes(color = raceNameShort, linetype = sex)) +
    ylim(62, 93) +
    scale_x_continuous(minor_breaks = myBreaks,
                       breaks = myBreaks,
                       expand = expansion(mult = c(0, 0), add = c(1, 5)), 
                       labels = myLabels) +
    scale_color_manual(values = plot_colors$raceNameShort) +   
    labs(title = myTitle, y = "Life Expectancy at Birth", x = "Year")  +
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=1), 
          legend.position = "none") +    
    geom_dl(aes(label = lineLabel, color=raceNameShort), 
            method = list(dl.trans(x = x + 0.2), "last.points", cex = ggplot_standards$lineLabelSize, 'bumpup', font = "bold")) 
  
  if (myCI) {
    tplot <- tplot +
      geom_line(data = tDat, aes(x = year, y = exlow, color = raceNameShort, linetype = sex)) +
      geom_line(data = tDat, aes(x = year, y = exhigh, color = raceNameShort, linetype = sex)) 
  }
  
  # Table Setup ==============================================
  
  tDat <- tDat %>% 
    mutate(ex = round(ex,2), 
           exlow = round(exlow,2),
           exhigh = round(exhigh,2)) %>%
    select(county, nyrs, year, sex, raceNameShort, LifeExpectancy=ex, LECI_lower = exlow, LECI_upper = exhigh)
  
  # Return
  list(plotL = tplot, dataL = tDat)
  
}


# Test Function
if (F) {
  LEtrend(myLHJ="CALIFORNIA", 
          mySexMult = c("Female", "Male"), 
          myRace = c("Asian", "Black", "Latino", "White"), 
          myCI = F, 
          myYearGrouping = 1)
}

