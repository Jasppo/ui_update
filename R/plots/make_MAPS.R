ccbMap <- function(myLHJ = "Alameda", 
                   myCause = "0", 
                   myMeasure = "YLLper",  
                   myYear = 2021,
                   mySex  = "Total",   
                   myStateCut = TRUE,  
                   myGeo = "Community", 
                   myCutSystem ="Fisher") {
  
  
  # Testing =====================================================
  
  if (F){
    myLHJ <- "Alameda"
    myCause <- "0"
    myMeasure <- "aRate"
    myYear <- 2021
    mySex <- "Total"
    myStateCut <- TRUE
    myGeo <- "Community"
    myCutSystem <- "Quantile" 
  }
  
  
  # Stop messages ========================
  
  if (myLHJ %in% constants$cityLHJs) {
    stop(appText$cityMessage)
  }
  
  if( myGeo %in% c("Community", "Census Tract") & myMeasure == "SMR" ) stop('Sorry kid, SMR calculated only for County level')
  
  # Setup =========================================
  
  deathMeasures <- choices$deathMeasuresAll
  measureName <- names(deathMeasures)[deathMeasures == myMeasure]
  
  
  # Load and Process Data ============================
  
  # Projection to transform shape files to
  proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  if (myGeo == "County") {
    
    shapeData <- lazy_load("myData/shape_files/shape_County.shp")
    shapeData_trans <- st_transform(shapeData, crs = proj2)
    
    datCounty <- lazy_load(file.path("myData", DATATYPE, "datCounty.RDS"))
    
    datState <- datCounty %>% 
      filter(year %in% (myYear - 4):myYear, sex == mySex, causeCode == myCause, county != constants$STATE) %>% 
      mutate(measure = !!as.symbol(myMeasure))
    
    dat1 <- datCounty %>% 
      filter(year==myYear, sex==mySex, causeCode==myCause, county != constants$STATE) %>% 
      mutate(geoLab = county, 
             measure = !!as.symbol(myMeasure))
      
    map1 <- shapeData_trans %>% 
      left_join(dat1, by = "county")
    
    geoTitle <- "County"
    
  } else if (myGeo == "Community") {
    
    shapeData <- lazy_load("myData/shape_files/shape_Comm.shp")
    
    datComm <- lazy_load(file.path("myData", DATATYPE, "datComm.RDS"))
    
    datState <- datComm %>% 
      filter(yearG5==current_years$currentYearG5, sex==mySex, causeCode==myCause, comID != "Unknown") %>% 
      rename(year = yearG5) %>% 
      mutate(geoLab = str_wrap(comName,15), 
             measure = !!as.symbol(myMeasure))
    
    dat1 <- datState
    
    if (myLHJ != constants$STATE) {
      shapeData <- shapeData %>% 
        filter(county == myLHJ)
      
      dat1 <- dat1 %>% 
        filter(county == myLHJ)
    }
    
    shapeData_trans <- st_transform(shapeData, crs = proj2)

    map1 <- shapeData_trans %>% 
      left_join(dat1, by = c("county", "comID"))
    
    geoTitle <- "Comm"
    
  } else if (myGeo == "Census Tract") {
    
    shapeData <- lazy_load("myData/shape_files/shape_Tract.shp")
    
    datTract <- lazy_load(file.path("myData", DATATYPE, "datTract.RDS"))
    
    datState <- datTract %>% 
      filter(yearG5==current_years$currentYearG5, sex==mySex, causeCode==myCause) %>% 
      rename(year = yearG5) %>% 
      mutate(geoLab = GEOID, 
             measure = !!as.symbol(myMeasure))
    
    dat1 <- datState
    
    if (myLHJ != constants$STATE) {
      
      shapeData <- shapeData %>% 
        filter(county == myLHJ)
      
      dat1 <- dat1 %>% 
        filter(county == myLHJ)
    }
    
    shapeData_trans <- st_transform(shapeData, crs = proj2)
    
    map1 <- shapeData_trans %>% 
      left_join(dat1, by = c("county", "GEOID"))
    
    geoTitle <- "Tract"
    
  }
  
  if (nrow(dat1)==0) stop(appText$stopMessage)
  
  dat1 <- dat1 %>% 
    left_join(select(links$deathCause, causeCode, causeNameShort), by = "causeCode")
  
  
  mapData <- map1 %>% 
    left_join(select(links$deathCause, causeCode, causeNameShort), by = "causeCode") %>% 
    mutate(measureLabel = comma(measure, accuracy = 1),
           mapLabel = paste0("<b>", geoTitle, "</b>: ", geoLab, 
                             "<br><b>Year</b>: ", year, 
                             "<br><b>Cause</b>: ", causeNameShort, 
                             "<br><b>Sex</b>: ", sex, 
                             "<br><b>Population</b>: ", scales::comma(population, accuracy = 1), 
                             "<br><b>", measureName, "</b>: ", measureLabel
         )) %>%  
    select(county, geoLab, measure, measureLabel, mapLabel)
  
  
  # Create Map ===============================================================
  
  ## Colors -------------------
  
  # Pull vector of measure values based on state cut argument
  if (myStateCut) {
    myRange <- datState$measure
  } else {
    myRange <- mapData %>%  
      st_drop_geometry() %>% 
      pull(measure)
  }
  
  myRange <- unique(na.omit(myRange))
  
  if (length(myRange) <= 1) {
    stop("Sorry friend, there are either only suppressed values to map or not enough values to use county-based cutpoints. Please use state-based cutpoints.")
  }
  
  # Map breaks - Either quantile or fisher
  
  if (myCutSystem == "Quantile") {
    myBreaks <- unname( quantile(myRange, probs = 0:5/5) )
  } else {
    myBreaks <- classIntervals(myRange, style = "fisher", breaks=NULL, n=5)$brks
  }
  
  # Function for making palette (with alpha)
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  # Create palette
  myPal <- brewer.pal(5,"Blues") 
  myPal <- add.alpha(myPal,.8)
  
  # Reverse palette if mean.age measure is selected
  if (myMeasure == "mean.age") myPal <- rev(myPal)
  
  # Creates a function to pass into leaflet map
  # Here, using color palette and myBreaks
  pal <- colorBin(myPal, domain = mapData$measure, bins = myBreaks)
  
  
  ## Create the leaflet map ---------------------
  
  tPlot <- leaflet(data = mapData) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%  # Add a basemap
    addPolygons(
      fillColor = ~pal(measure),  # Map colors using the palette
      weight = 1,  # Border weight
      color = "black",  # Border color
      fillOpacity = 0.7,  # Transparency
      popup = ~mapLabel  # Add popups showing values
    ) %>%
    addLegend(
      pal = pal, 
      values = ~measure, 
      title = measureName, 
      position = "bottomright"
    )

  # Create data for downloading ============================================
  
  varsIn <- c("year", "county")
  
  if (myGeo == "Community") {
    varsIn <- c(varsIn, "comID", "comName")
  } else if (myGeo == "Census Tract") {
    varsIn <- c(varsIn, "GEOID")
  }
  
  varsIn <- c(varsIn, "sex", "causeNameShort", myMeasure)
  
  tabDat  <- dat1 %>% 
    select(all_of(varsIn))
  
  list(tplot = tPlot, dataL = tabDat)
  
}