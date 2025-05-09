rankCause <- function(myCounty = "Los Angeles", 
                      myMeasure = "Ndeaths", 
                      myYear = 2021, 
                      mySex = "Total", 
                      myLev = "lev2", 
                      myN = 10, 
                      myMeanAge_sort = "Youngest to Oldest"){ 

  # Testing =============================
  if (F) {
    myCounty <- 'Los Angeles'
    myMeasure <- 'Ndeaths'
    myYear <- 2018
    mySex <- 'Total'
    myLev <- 'lev2'
    myN <- 10
    myMeanAge_sort <- "Youngest to Oldest"
  }
  
  if(myLev == "lev3") myLev <- c("lev2", "lev3")
  
  deathMeasures <- choices$deathMeasures_sort
  
  measuresLink <- data.frame(
    measure = unname(deathMeasures), 
    measureName = names(deathMeasures)
  )
  
  # -- CREATE PLOT DATAFRAME -------------------------------------------------------------------------------------------------
  
  # Filter based on user inputs, and pivot longer measures
  tDat <- lazy_load(file.path("myData", DATATYPE, "datCounty.RDS")) %>%
    filter(year == myYear, sex == mySex, Level == myLev, county == myCounty) %>%
    pivot_longer(cols = c("Ndeaths", "YLLper", "aRate", "mean.age", "SMR"), names_to = "measure", values_to = "value") %>%
    left_join(select(links$deathCause, causeCode, causeNameShort), by = "causeCode") %>%
    select(year, sex, Level, causeNameShort, county, measure, value)
  
  # Pull top N causes for user-specified measure
  if (myMeasure == "mean.age" & myMeanAge_sort == "Youngest to Oldest") {
    top_N_causes <- tDat %>%
      filter(measure == myMeasure) %>%
      arrange(value) %>%
      dplyr::slice(1:myN) %>%
      pull(causeNameShort)
  } else {
    top_N_causes <- tDat %>%
      filter(measure == myMeasure) %>%
      arrange(desc(value)) %>%
      dplyr::slice(1:myN) %>%
      pull(causeNameShort)
  }
  
  
  # Filter on top N causes to create final plot dataset
  plotData <- tDat %>%
    filter(!is.na(causeNameShort), causeNameShort %in% top_N_causes) %>%
    left_join(measuresLink, by = "measure") %>%
    mutate(causeNameShort = factor(causeNameShort, levels = rev(top_N_causes)), 
           measureName    = factor(measureName,    levels = names(deathMeasures))) 
  
  
  # --- CREATE PLOT -------------------------------------------------------------------------------------------------------------
  
  # Plot Title
  sexLab <- ""
  if (mySex != "Total") sexLab <- paste0(", among ", mySex, "s")
  tit <- paste0("Measures by Cause in ", myYear, " in ", myCounty, sexLab)
  
  # Remove SMR if California
  if (myCounty == "CALIFORNIA") {
    plotData <- plotData %>% filter(measureName != "Standard Mortality Ratio")
  }
  
  rankCause_plot <- ggplot(plotData, aes(x = causeNameShort, y = value)) +
    geom_bar(stat = "identity", fill = ggplot_standards$barColor) +
    coord_flip() +
    facet_grid(. ~ measureName, scales = "free_x", labeller=labeller(measureName = label_wrap_gen(5))) + 
    scale_x_discrete(labels = scales::wrap_format(40)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = tit) +
    theme(axis.title   = element_blank(), 
          axis.text.x  = element_text(face="bold",angle = 90, hjust = 1, vjust = 0.5))
  
  if (myCounty != constants$STATE) {
    
    rankCause_plot <- rankCause_plot +
      
      geom_hline(data = data.frame(yint = 1,   measureName = "Standard Mortality Ratio"), aes(yintercept = yint, color="State Average"),         linetype="dashed", size = 1) + # adds SMR line only to SMR facet
      geom_hline(data = data.frame(yint = 0.8, measureName = "Standard Mortality Ratio"), aes(yintercept = yint, color="80% of State Average"),  linetype="dashed", size = 1) +
      geom_hline(data = data.frame(yint = 1.2, measureName = "Standard Mortality Ratio"), aes(yintercept = yint, color="120% of State Average"), linetype="dashed", size = 1) +
      scale_colour_manual(name="", values = c("State Average" = "grey", "80% of State Average" = "green", "120% of State Average" = "red")) +
      theme(legend.position = "bottom", legend.justification = "right", legend.text=element_text(size=15), legend.key.width = unit(1.5,"cm"), 
            legend.direction='vertical')
    
    
  }
  
  
  # Clean up data frame for downloading
  
  plotData <- plotData %>%
    select(year, sex, county, measureName, value, causeNameShort)
  
  
  list(plotL = rankCause_plot, dataL = plotData)
  
  
}


# Notes about adding line to single facet area: https://stackoverflow.com/questions/34686217/how-can-i-add-a-line-to-one-of-the-facets