maps_ui_body <- function(id) {
  leafletOutput("mapsTab_map", height = "80vh")
}

maps_server <- function(id, input, output, session, currentTab) {

  # Rules for inputs ===========================================================
  
  # If myLHJ is not CA and geoLevel is County, change geoLevel to Community
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myLHJ != constants$STATE & input$myGeo == "County") {
      isolate(updateSelectInput(inputId = "myGeo", selected = "Community")) # isolate ensures the observe does not re-run after updating myGeo
      }
    
    })
  
  # Observe geoLevel:
  # If geoLevel = County, then show yearSlider and hide Tract help text
  # If geoLevel = Community, then hide yearSlider and hide Tract help text
  # If geoLevel = Tract, then hide yearSlider and show Tract help text
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myGeo == "County") {
      
      shinyjs::show("myYear")
      shinyjs::hide("myGeoHelpText")
      
      } else if (input$myGeo == "Community") {
        
        shinyjs::hide("myYear")
        shinyjs::hide("myGeoHelpText")
        
        } else if (input$myGeo == "Census Tract") {
          
          shinyjs::hide("myYear")
          shinyjs::show("myGeoHelpText")

        }
    
    })
  
  
  # Render map ==================================================================
  
  # Create a debounced version of the inputs
  debounced_inputs <- reactive({
    list(
      myLHJ = input$myLHJ, 
      myCause = input$myCAUSE, 
      myMeasure = input$myMeasure,  
      myYear = input$myYear,
      mySex  = input$mySex,   
      myStateCut = input$myStateCut,  
      myGeo = input$myGeo, 
      myCutSystem = input$myCutSystem
    )
  }) %>% debounce(300) # 100 ms delay
  
  plot_step <- reactive({
    
    inputs <- debounced_inputs()
    
    # If geoLevel is Tract and PH/Detail level cause is selected, return error message
    validate(
      need(!(inputs$myGeo == "Census Tract" && nchar(inputs$myCause) > 1),
           "Public health and detail level causes are not available at the Census Tract level. Please choose a broad condition or 'All Causes'.")
    )
    
    # If geoLevel is Community and Detail level cause is selected, return error message
    validate(
      need(!(inputs$myGeo == "Community" && nchar(inputs$myCause) == 4),
           "Detail level causes are not available at the Community level. Please choose a non-detail level cause.")
    )
    
    ccbMap(myLHJ = inputs$myLHJ, 
           myCause = inputs$myCause, 
           myMeasure = inputs$myMeasure,  
           myYear = inputs$myYear,
           mySex  = inputs$mySex,   
           myStateCut = inputs$myStateCut,  
           myGeo = inputs$myGeo, 
           myCutSystem = inputs$myCutSystem)
  })
  
  output$mapsTab_map <- renderLeaflet(plot_step()$tplot)
  
  
  # Download data ==============================================================
  
  download(myID = id, 
           output = output, 
           myData = reactive(plot_step()$dataL), 
           myChart = NULL, 
           myWidth = 18, 
           myHeight = 10, 
           myPointSize = 10, 
           myRes = 100)
  
}