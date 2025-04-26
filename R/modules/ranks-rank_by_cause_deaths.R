rankCause_ui_body <- function(id) {
  plotOutput("rankByCause_plot")
}

rankCause_server <- function(id, input, output, session, currentTab) {
  
  # Rules for inputs ===========================================================
  
  # Show/hide myMeanAge_sort based on myMeasure selection
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myMeasure_sort == "mean.age") {
      shinyjs::show("myMeanAge_sort")
    } else {
      shinyjs::hide("myMeanAge_sort")
    }
    
  })

  
  # Render plot ====================================
  
  # Create a debounced version of the inputs
  debounced_inputs <- reactive({
    list(
      myLHJ = input$myLHJ, 
      myMeasure_sort = input$myMeasure_sort,
      myMeanAge_sort = input$myMeanAge_sort,
      myYear = input$myYear,
      mySex  = input$mySex,   
      myN = input$myN,
      myLev = input$myLev
    )
  }) %>% debounce(300) # 100 ms delay
  
  plot_step <- reactive({
    
    inputs <- debounced_inputs()
    
    # If lhj = CA and measure sort = SMR, return error message
    validate(
      need(!(inputs$myLHJ == constants$STATE && inputs$myMeasure_sort == "SMR"),
           "Standard Mortality Ratio is not available for California. Please select an LHJ or another measure to sort by.")
    )
    
    rankCause(myCounty = inputs$myLHJ, 
              myMeasure = inputs$myMeasure_sort, 
              myYear = inputs$myYear, 
              mySex = inputs$mySex, 
              myLev = inputs$myLev, 
              myN = inputs$myN, 
              myMeanAge_sort = inputs$myMeanAge_sort)
    
  })
  
  output$rankByCause_plot <- renderPlot(plot_step()$plotL)
  
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