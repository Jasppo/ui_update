disparities_ui_body <- function(id) {
  
  tagList(
    uiOutput("disparitiesTab_title"), 
    layout_columns(
      col_widths = c(8, 4, 12),
      row_heights = c(1, 1),
      
      card(full_screen = T, class = "bg-light", 
           card_header("Race/Ethnicity"), 
           card_body(highchartOutput("disparitiesTab_racePlot", height = "35vh"))
           ),
      
      card(full_screen = T, class = "bg-light",
           card_header("Sex"), 
           card_body(highchartOutput("disparitiesTab_sexPlot", height = "35vh"))
      ),
      
      card(full_screen = T, class = "bg-light",
           card_header("Age Group"), 
           card_body(highchartOutput("disparitiesTab_agePlot", height = "35vh"))
      )
    )
  )
}

disparities_server <- function(id, input, output, session, currentTab) {
  
  # Set up reactive values for plot title ===================
  
  # Year
  disparitiesTab_year_title_val <- reactiveVal(as.character(current_years$currentYear))
  
  # Cause of Death
  disparitiesTab_cause_title_val <- reactiveVal("All CAUSES")
  
  
  # Update reactive values when myYearGrouping input changes =====================
  observe({
    
    req(currentTab$tab == id)
    
    new_year_val <- case_when(input$myYearGrouping == 1 ~ as.character(current_years$currentYear),
                              input$myYearGrouping == 3 ~ as.character(current_years$currentYearG3),
                              input$myYearGrouping == 5 ~ as.character(current_years$currentYearG5)
    )
    disparitiesTab_year_title_val(new_year_val)
    
    new_cause_val <- links$deathCause %>% 
      filter(causeCode == input$myCAUSE) %>% 
      pull(causeName)
    disparitiesTab_cause_title_val(new_cause_val)
    
  })
  
  
  # Dynamic Titles =====================================
  output$disparitiesTab_title <- renderUI({
    h2(paste0("Disparities in Death Rates in ", disparitiesTab_cause_title_val(), " in ", input$myLHJ, ", ", disparitiesTab_year_title_val()))
  })
  
  
  # Render plot ====================================
  
  # Create a debounced version of the inputs
  debounced_inputs <- reactive({
    list(
      myLHJ = input$myLHJ,
      myCause = input$myCAUSE,
      myYearGrouping = input$myYearGrouping,
      myCompare = input$myCompare
    )
  }) %>% debounce(300) # 100 ms delay
  
  plot_step <- reactive({
    
    inputs <- debounced_inputs()
    
    # If myYearGrouping = 5, return error message
    validate(
      need(inputs$myYearGrouping != 5,
           "5-year grouping is not available in this view. Please select 1 or 3-year grouping for state, or 3-year grouping for LHJ.")
    )
    
    # If myYearGrouping = 1 and LHJ selected, return error message
    validate(
      need(!(inputs$myLHJ != constants$STATE && inputs$myYearGrouping == 1),   
           "1-year grouping is not available for LHJs. Please select 3-year grouping.")
    )
    
    
    disparity(myLHJ = inputs$myLHJ,
              myCause = inputs$myCause,
              myYearGrouping = inputs$myYearGrouping,
              myCompare = inputs$myCompare
    )
  })
  
  output$disparitiesTab_racePlot <- renderHighchart(plot_step()$plotL_race)
  output$disparitiesTab_sexPlot <- renderHighchart(plot_step()$plotL_sex)
  output$disparitiesTab_agePlot <- renderHighchart(plot_step()$plotL_age)
  
  # Download data and chart ==============================================================
  
  download(myID = id, 
           output = output, 
           myData = reactive(plot_step()$dataL), 
           myChart = NULL, 
           myWidth = 18, 
           myHeight = 10, 
           myPointSize = 10, 
           myRes = 100)
  
}