trends_demo_ui_body <- function(id) {
  plotOutput("trends_demo_plot")
}

trends_demo_server <- function(id, input, output, session, currentTab) {
  
  # Rules for inputs ===========================================================
 
  # Show/hide inputs based on myDemoGroup selection
  observe({
    
    req(currentTab$tab == id)
    
    if (input$myDemoGroup == "Sex") {
      shinyjs::show(id = "myYearGrouping")
      shinyjs::hide(id = "myMultiRace")
      shinyjs::hide(id = "myMultiRaceNote")
      
    } else if (input$myDemoGroup == "Age Group") {
      shinyjs::show(id = "myYearGrouping")
      shinyjs::hide(id = "myMultiRace")
      shinyjs::hide(id = "myMultiRaceNote")
      
    } else if (input$myDemoGroup == "Race/Ethnicity") {
      shinyjs::show(id = "myYearGrouping")
      shinyjs::show(id = "myMultiRace")
      
    } else if (input$myDemoGroup == "Education") {
      shinyjs::hide(id = "myYearGrouping")
      shinyjs::hide(id = "myMultiRace")
      shinyjs::hide(id = "myMultiRaceNote")
    }
    
  })
  
  
  # Show MultiRaceNote if MultiRace is selected
  observe({
    
    req(currentTab$tab == id)
    req(input$myDemoGroup == "Race/Ethnicity")
    
    if (input$myMultiRace) {
      shinyjs::show(id = "myMultiRaceNote")
    } else {
      shinyjs::hide(id = "myMultiRaceNote")
    }

  })
  
  # Render plot ====================================
  
  # Create a debounced version of the inputs
  debounced_inputs <- reactive({
    list(
      myDemoGroup = input$myDemoGroup,
      myLHJ = input$myLHJ,
      myCause = input$myCAUSE,
      myMeasure = input$myMeasure,
      mySex = "Total",
      myYearGrouping = input$myYearGrouping,
      myLogTrans = input$myLogTrans
    )
  }) %>% debounce(300) # 100 ms delay
  
  plot_step <- reactive({
    
    inputs <- debounced_inputs()
    
    # Return error: If myMeasure == SMR
    validate(
      need(inputs$myMeasure != "SMR",
           "Standard Mortality Ratio is not available in this view. Please select another measure.")
    )
    
    # Return error: If myDemoGroup == Age Group & myMeasure == aRate
    validate(
      need(!(inputs$myDemoGroup == "Age Group" && inputs$myMeasure == "aRate"),
           "Age-Adjusted measures are not available for age group trends. Please select another measure.")
    )
    
    # Return error: If myLHJ == county/city & myDemoGroup in Age Group or Race/Ethnicity & myYearGroup in 1, 5
    validate(
      need(!(inputs$myLHJ != constants$STATE && inputs$myDemoGroup %in% c("Age Group", "Race/Ethnicity") && inputs$myYearGrouping %in% c(1, 5)),
           "Only 3-year age group and race/ethnicity trends are available for LHJs. Please choose '3' for 'Years to Group.'")
    )
    
    # Return error: If myLHJ == STATE & myDemoGroup in Age Group or Race/Ethnicity & myYearGroup == 5
    validate(
      need(!(inputs$myLHJ == constants$STATE && inputs$myDemoGroup %in% c("Age Group", "Race/Ethnicity") && inputs$myYearGrouping == 5),
           "5-year age group and race/ethnicity statewide trends are not available. Please choose '1' or '3' for 'Years to Group.'")
    )
    
    trendGeneric(myDemoGroup = inputs$myDemoGroup, 
                 myLHJ = inputs$myLHJ, 
                 myCause = inputs$myCause, 
                 myMeasure = inputs$myMeasure,
                 mySex = inputs$mySex,
                 myYearGrouping = inputs$myYearGrouping,
                 myLogTrans = inputs$myLogTrans,
                 myMultiRace = input$myMultiRace
    )
  })
  
  
  output$trends_demo_plot <- renderPlot(plot_step()$plotL)
  
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
  