rankGeo_ui_body <- function(id) {
    highchartOutput("rankByGeoTab_plot", height = "80vh")
}


rankGeo_server <- function(id, input, output, session, currentTab) {
  
  # Rules for inputs ===========================================================
  
  # Show/hide myCI based on measure selection
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myMeasure %in% c("cDeathRate", "aRate")) {
      shinyjs::show("myCI")
    } else {
      shinyjs::hide("myCI")
    }
  })
  
  # Show/hide myYear based on LHJ selection
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myLHJ == constants$STATE) {
      shinyjs::show("myYear")
    } else {
      shinyjs::hide("myYear")
    }
  })
  
  # Render plot ====================================
  
  # Create a debounced version of the inputs
  debounced_inputs <- reactive({
    list(
      myLHJ = input$myLHJ, 
      myCause = input$myCAUSE, 
      myMeasure = input$myMeasure, 
      myYear = input$myYear,
      mySex = input$mySex, 
      myCI = input$myCI,
      myRefLine = input$myRefLine
    )
  }) %>% debounce(300) # 100 ms delay
  
  plot_step <- reactive({
    
    inputs <- debounced_inputs()
    
    # If measure is SMR, return error message
    validate(
      need(inputs$myMeasure != "SMR",
           "Standard Mortality Ratio is not available in this view. Please select a different measure.")
    )
    
    # If LHJ is city LHJ, return error message
    validate(
      need(!inputs$myLHJ %in% constants$cityLHJs,
           appText$cityMessage)
    )
    
    # If LHJ is not CA and Detail level cause is selected, return error message
    validate(
      need(!(inputs$myLHJ == constants$STATE && nchar(inputs$myCause) == 4),
           "Detail level causes are not available at the community level. Please choose a non-detailed level cause.")
    )
    
    rankGeo(myLHJ = inputs$myLHJ, 
            myCause = inputs$myCause, 
            myMeasure = inputs$myMeasure, 
            myYear = inputs$myYear,
            mySex = inputs$mySex, 
            myCI = inputs$myCI,
            myRefLine = inputs$myRefLine
    )
  })
  
  
  output$rankByGeoTab_plot <- renderHighchart(plot_step()$plotL_i)
  
  # Download data and chart ==============================================================
  
  download(myID = id, 
           output = output, 
           myData = reactive(plot_step()$dataL), 
           myChart = reactive(plot_step()$plotL), 
           myWidth = 18, 
           myHeight = 10, 
           myPointSize = 10, 
           myRes = 100)
  
}



