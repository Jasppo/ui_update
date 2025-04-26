# Population Pyramid ======================================================

make_demoPop_Pyramid <- function(myCounty, myYear) {

  # Test
  if (F) {
    myCounty <- "CALIFORNIA"
    myYear <- 2021
  }
  
  # Sex Colors
  sex_colors <- data.frame(sex = names(plot_colors$gender), 
                           color = unname(plot_colors$gender))
  
  # Make 5Y age groups - factor
  age5 <- data.frame(lAge = seq(0, 100, by = 5)) %>% 
    mutate(uAge = lAge + 4, 
           ageGroup = paste0(lAge, " - ", uAge), 
           ageGroup = ifelse(ageGroup == "100 - 104", "100+", ageGroup))

  tDat <- lazy_load("myData/population/popData_AgePyramid.RDS") %>% 
    filter(county == myCounty, year == myYear) %>% 
    mutate(population_notAbs = ifelse(sex == "Male", -1 * population, population),
           plotText = paste0("<b>Sex:</b> ", sex, 
                             "<br/><b>Age Group:</b> ", ageGroup, 
                             "<br/><b>Population:</b> ", scales::comma(population, accuracy = 1)), 
           ageGroup = factor(ageGroup, levels = age5$ageGroup)) %>% 
    left_join(sex_colors, by = "sex") %>% 
    arrange(desc(ageGroup)) 
  
  tDat_male <- tDat %>% filter(sex == "Male")
  tDat_female <- tDat %>% filter(sex == "Female")
  
  tPlot <- highchart() %>% 
    hc_xAxis(categories = tDat_male$ageGroup, 
             labels = list(step = 1)) %>% 
    hc_yAxis(labels = list(formatter = JS(
      "function() {
      return Highcharts.numberFormat(Math.abs(this.value), 0);
      }"
    ))) %>% 
    hc_add_series(name = "Male", data = tDat_male$population_notAbs, type = "bar") %>% 
    hc_add_series(name = "Female", data = tDat_female$population_notAbs, type = "bar") %>%
    hc_plotOptions(series = list(stacking = "normal")) %>% 
    hc_tooltip(
      useHTML = TRUE,  # Enable HTML formatting
      formatter = JS(
        "function() {
        return '<b>Sex:</b> ' + this.series.name + 
        '<br/><b>Age Group:</b> ' + this.point.category + 
        '<br/><b>Population:</b> ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
      }"
      )
  ) 

  list(plotL = tPlot)

}

# Pie chart - Race/Ethnicity =======================================

make_demoPop_RacePie <- function(myCounty, myYear) {
  
  # Testing
  if (F) {
    myCounty <- "CALIFORNIA"
    myYear <- 2021
  }
  
  colors_df <- data.frame(
    raceNameShort = names(plot_colors$raceNameShort), 
    plot_color = plot_colors$raceNameShort
  )
   
  tDat <- lazy_load("myData/population/popData_RacePie.RDS") %>% 
    filter(county == myCounty, year == myYear) %>% 
    mutate(p = 100 * population / sum(population)) %>% 
    left_join(colors_df, by = "raceNameShort")
  
  plotTitle <- ifelse(myCounty == constants$STATE, paste0("Population by Race/Ethnicity \nin ", myCounty, ", ", myYear),
                      paste0("Population by Race/Ethnicity \nin ", myCounty, " County, ", myYear))
  
  raceColors <- plot_colors$raceNameShort[names(plot_colors$raceNameShort) %in% unique(tDat$raceNameShort)]
  
  myPlot <- hchart(tDat, type = "pie", hcaes(y = p, name = raceNameShort)) %>% 
    hc_tooltip(
      useHTML = TRUE,  # Enable HTML formatting
      formatter = JS(
        "function() {
        return '<b>' + this.point.name + '</b><br/>' +  
               'Population: ' + Highcharts.numberFormat(this.point.population, 0) + '<br/>' +  
               'Percent: ' + Highcharts.numberFormat(this.point.y, 1) + '%';
      }"
      )
    ) %>% 
    hc_plotOptions(pie = list(colorByPoint = TRUE, 
                              colors = tDat$plot_color, 
                              dataLabels = list(
                                enabled = TRUE, 
                                format = '{point.name}: {point.y:.1f}%'
                              )
                              ))
  
  list(plotL = myPlot)
  
  
}


# Bar Chart Race by Age ===============================================

make_demoPop_RaceAge <- function(myCounty, myYear) {
  
  # Test 
  if (F) {
    myCounty <- "CALIFORNIA"
    myYear <- 2021
  }
  
  t_raceLink <- links$race %>% 
    select(raceCode, raceNameShort) %>% 
    filter(!raceCode %in% c("Other", "Total", "Unknown"))
  
  tDat <- lazy_load("myData/population/popData_RaceAge.RDS") %>% 
    filter(county == myCounty, year == myYear) %>% 
    group_by(raceName) %>%
    mutate(percent = round(100*population/sum(population), 1)) %>%
    ungroup() %>% 
    mutate(raceNameShort = factor(raceNameShort, levels = unique(t_raceLink$raceNameShort)))
    
  ageColors <- data.frame(ageGroup = unique(tDat$ageGroup), 
                          ageColor = plot_colors$palette[1:length(unique(tDat$ageGroup))])
  
  tDat <- tDat %>%
    left_join(ageColors, by = "ageGroup") %>% 
    mutate(ageGroup = factor(ageGroup, levels = rev(ageColors$ageGroup)))
  
  tPlot <- hchart(tDat, "bar", hcaes(x = raceNameShort, y = percent, group = ageGroup)) %>% 
    hc_plotOptions(series = list(stacking = "normal")) %>% 
    hc_tooltip(
      useHTML = TRUE,  # Enable HTML formatting
      formatter = JS(
        "function() {
        return '<b>Race/Ethnicity:</b> ' + this.point.raceNameShort + 
        '<br/><b>Age Group:</b> ' + this.point.ageGroup +
        '<br/><b>Population:</b> ' + Highcharts.numberFormat(this.point.population, 0) + 
        '<br/><b>Percent:</b> ' + Highcharts.numberFormat(this.point.y, 1) + '%';
      }"
      )) %>% 
    hc_legend(reversed = T, title = list(text = "Age Group"), 
              layout = 'vertical', align = 'right', verticalAlign = 'top') %>% 
    hc_yAxis(title = list(text = "Percent"), max = 100) %>% 
    hc_xAxis(title = list(text = "Race/Ethnicity")) 
  
  
  
  list(plotL = tPlot)
  
  
}


# Trends =======================================================================

# Trends function
make_demoPop_trend <- function(myCounty, myGroup = "Total") {
  
  # Test
  if (F) {
    myLHJ <- "CALIFORNIA"
    myGroup <- "Age Group"
  }
  
  popData_trends <- lazy_load("myData/population/popData_SexRaceAge_Trends.RDS")

  # Total Trend
  if (myGroup == "Total") {
    
    tDat <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup == "Total") %>% 
      mutate(strata = sex)
    
  } else if (myGroup == "Sex") {
    
    tDat <- popData_trends %>%
      filter(county == myCounty, sex != "Total", raceName == "Total", ageGroup == "Total") %>% 
      mutate(strata = sex)
    
  } else if (myGroup == "Race/Ethnicity") {
    
    tDat <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName != "Total", ageGroup == "Total") %>% 
      mutate(strata = raceNameShort)
    
  } else if(myGroup == "Age Group") {
    
    tDat <- popData_trends %>%
      filter(county == myCounty, sex == "Total", raceName == "Total", ageGroup != "Total") %>% 
      mutate(strata = ageGroup)
    
  }
  
  myPlot <- hchart(tDat, "line", hcaes(x = year, y = population, group = strata)) %>% 
    hc_xAxis(title = list(text = "Year")) %>% 
    hc_yAxis(title = list(text = "Population")) %>% 
    hc_tooltip(shared = TRUE, crosshairs = TRUE)
  
  
  list(plotL = myPlot)
  
}


