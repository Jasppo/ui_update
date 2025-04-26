rankGeo <- function(myLHJ="CALIFORNIA", 
                    myCause="0", 
                    myMeasure = "YLL", 
                    myYear=2021,
                    mySex="Total", 
                    myCI=TRUE,
                    myRefLine=FALSE) {
  
  
  # Testing
  if (1==2){
    myLHJ = "Alameda"
    myCause="E02"
    myMeasure = "aRate"
    myYear <- 2021
    mySex = "Total"
    myCI <- TRUE
    myRefLine <- TRUE
  }
  
  # If City is chosen, stop and return message
  if (myLHJ %in% constants$cityLHJs) {
    stop(appText$cityMessage)
  }
  
  # Labels =========================================================
  
  # Measure Name
  measures <- choices$deathMeasuresAll
  measureName <- names(measures[measures == myMeasure])
  
  # Cause Name
  causeLab <- links$deathCause %>% 
    filter(causeCode == myCause) %>% 
    pull(causeNameShort)
  
  # Sex
  sexLab  <- ""
  if (mySex != "Total") sexLab <- paste0(", among ", mySex, "s")
  
  
  # Load and process data ========================================
  
  if (myLHJ == constants$STATE) {
    
    # Load and process county level data
    tDat <- lazy_load(file.path("myData", DATATYPE, "datCounty.RDS")) %>% 
      mutate(plotter = !!as.symbol(myMeasure)) %>%
      filter(year == myYear, sex==mySex, causeCode==myCause) %>% 
      arrange(!is.na(plotter), plotter) %>% 
      mutate(lab = county)
    
    # State Reference Line
    sMeasure <- tDat %>% 
      filter(county == constants$STATE) %>% 
      pull(plotter)
    
    # Plot title
    tit <- paste0("County Ranking of ", 
                  measureName,
                  " for ", 
                  causeLab,
                  " in ",
                  myYear,
                  sexLab)
    
    
  } else {
    
    # County Reference Line
    sMeasure <- lazy_load(file.path("myData", DATATYPE, "datCounty_5year.RDS")) %>% 
      filter(county == myLHJ, yearG5 == current_years$currentYearG5, sex == mySex, causeCode == myCause) %>% 
      pull( {{ myMeasure }})
    
    # Load and process MSSA-Level data
    tDat <- lazy_load(file.path("myData", DATATYPE, "datComm.RDS")) %>% 
      mutate(plotter = !!as.symbol(myMeasure)) %>% 
      filter(county==myLHJ, yearG5==current_years$currentYearG5, sex==mySex, causeCode==myCause, comID != "Unknown") %>% 
      arrange(!is.na(plotter),plotter) %>%
      mutate(lab = str_wrap(comName, 30))
    
    # Plot title
    tit <- paste0("Community Ranking of ", 
                  measureName,
                  " for ", 
                  causeLab,
                  " in ",
                  myLHJ,
                  " in ",
                  current_years$currentYearG5,
                  sexLab)

  }
  
  # Static Plot ==================================================================
  
  rank_geo_plot <- ggplot(tDat, aes(x=reorder(lab, plotter), y=plotter)) +
    geom_bar(stat='identity', fill = 'gray', color = 'black') +
    coord_flip() +
    ggtitle(stringr::str_wrap(tit, 62)) +
    scale_y_continuous(sec.axis = dup_axis()) +
    theme_bw() + # element_text(size = rel(#)) below fixes the text scaling issue when specifying width and height of plot
    theme(plot.title = element_text(size = rel(2.5), colour = "blue"), 
          axis.title=element_blank(), 
          axis.text = element_text(size = rel(1.5)))
  
  # Reference Line
  refLabel <- ifelse(myLHJ == constants$STATE, "State Reference Line", "County Reference Line")
  
  if (myRefLine) {
    rank_geo_plot = rank_geo_plot + 
      geom_hline(yintercept=sMeasure, linetype="dotted", linewidth = 1) +
      geom_text(x = nrow(tDat), label=refLabel, y=sMeasure, 
                colour="black", angle=270, hjust = 0, vjust = 1.4, size=6)
  }
  
  # CI - Only present in crude and age-adjusted death rates
  if (myCI && myMeasure=="cDeathRate") { 
    
    rank_geo_plot = rank_geo_plot +
      geom_errorbar(aes(ymin = rateLCI, ymax = rateUCI), 
                    width = 0.5, color = "blue")
    
  } else if (myCI && myMeasure == "aRate") {
    
    rank_geo_plot = rank_geo_plot +
      geom_errorbar(aes(ymin = aLCI, ymax = aUCI), 
                    width = 0.5, color = "blue")
    
  }
  
  
  # Interactive Plot =====================================================
  
  # Further data prep
  tDat_i <- tDat %>% 
    arrange(desc(plotter)) %>% 
    mutate(color = ifelse(lab == constants$STATE, "firebrick", "#8ca48c"))
  
  # Determine fixed Y-axis range
  y_min <- 0
  y_max <- case_when(myCI && myMeasure == "cDeathRate" ~ max(tDat_i$rateUCI) * 1.1, 
                     myCI && myMeasure == "aRate" ~ max(tDat_i$aUCI) * 1.1,
                     TRUE ~ max(tDat_i$plotter) * 1.1
  )  
  

  tPlot_i <- hchart(tDat_i, "bar", hcaes(x = lab, y = plotter)) %>%
    hc_plotOptions(
      bar = list(colorByPoint = TRUE,
                    colors = tDat_i$color
                    ), 
      errorbar = list(color = "black", 
                      stemWidth = 1)
    ) %>%
    hc_title(text = tit) %>%  # Add a plot title
    hc_xAxis(title = list(text = ""), 
             scrollbar = list(enabled = TRUE),  # Enable scrollbar
             max = min(20, nrow(tDat_i))-1  # Show a maximum of 4 bars at a time (adjust as needed)
             ) %>%  
    hc_yAxis(title = list(text = measureName), 
             gridLineWidth = 1, 
             gridLineColor = "#f7f6f6",
             zoomType = "x", # Enable zooming
             min = y_min,  # Set fixed minimum value
             max = y_max   # Set fixed maximum value
             ) %>% 
    hc_chart(plotBackgroundColor = "#e7e6e6")
  
  # CI - Only present in crude and age-adjusted death rates
  if (myCI && myMeasure=="cDeathRate") { 
    
    tPlot_i <- tPlot_i %>% 
      hc_add_series(tDat_i, "errorbar", 
                    hcaes(x = lab, y = plotter, low = rateLCI, high = rateUCI)
      )
    
  } else if (myCI && myMeasure == "aRate") {
    
    tPlot_i <- tPlot_i %>% 
      hc_add_series(tDat_i, "errorbar", 
                    hcaes(x = lab, y = plotter, low = aLCI, high = aUCI)
      )
    
  }
  
  # Reference Line
  if (myRefLine) {
    
    tPlot_i <- tPlot_i %>% 
      hc_yAxis(
        plotLines = list(
          list(
            value = sMeasure,  # X-axis index where the line will be placed (adjust as needed)
            color = "black",  # Line color
            width = 2,  # Line width
            dashStyle = "Dash",  # Line style (e.g., "Solid", "Dash", "Dot")
            zIndex = 5,  # Ensure the line and label are on top
            label = list(
              text = refLabel,  # Label for the reference line
              align = "left",  # Label alignment
              verticalAlign = "top",  # Vertical alignment
              rotation = 90  # No rotation
            )
          )))
  }
  
  # Data Download ========================================================
  
  # Columns to select
  if (myLHJ == constants$STATE) {
    varsIn <- c("year", "county", "sex", "population", "causeName", myMeasure)
  } else {
    varsIn <- c("yearG5", "county", "comName", "sex", "population", "causeName", myMeasure)
  }
  
  # Add SE and CIs if selected
  if (myMeasure == "cDeathRate") {
    varsIn <- c(varsIn, "rateSE", "rateLCI", "rateUCI")
  } else if (myMeasure == "aRate") {
    varsIn <- c(varsIn, "aSE", "aLCI", "aUCI")
  }
  
  # Final data for download
  tDat <- tDat %>%
    left_join(select(links$deathCause, causeCode, causeName), by = "causeCode") %>%
    select(all_of(varsIn)) %>%
    arrange(desc(get(myMeasure)))
  
  list(plotL = rank_geo_plot, plotL_i = tPlot_i, dataL = tDat)
  
}

# Test
if (F) {
  rankGeo(myLHJ = "Alameda", 
          myCause="E02", 
          myMeasure = "aRate", 
          myYear=2021,
          mySex="Total", 
          myCI=TRUE,
          myRefLine=TRUE)$plotL_i
}