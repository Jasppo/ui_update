# Ranks -> Rank by Cause

# 1. UI - Sidebar
# 2. UI - Main Panel (plot)
# 3. Server
#   - Condition #1:
#     - If mean.age selected as measure -> show mean.age sort option
#     - Else hide mean.age sort option

rankCause_ui_sidebar <- function(id) {
  
  deathMeasuresShort_Dropdown <- choices$deathMeasuresAll
  deathMeasuresShort_Dropdown <- deathMeasuresShort_Dropdown[deathMeasuresShort_Dropdown %in% c("Ndeaths", "YLLper", "aRate", "mean.age", "SMR")]
  
  tagList(
    selectInput(NS(id, "myMeasure_sort"),  label=list(strong("Measure Sort Order:"), actionButton("measureHelp", label=inputStyles$helpIcon,style=inputStyles$myInputHelpButtonSty)),
                choices = deathMeasuresShort_Dropdown, selected = "aRate"),
    radioButtons(NS(id, "myMeanAge_sort"),  strong('Sort "Mean Age at Death" from:'),  
                 choices = c("Youngest to Oldest", "Oldest to Youngest")),
    sliderInput(NS(id, "myYear"), strong("Year:"), value=current_years$maxYear, min=2001, max=current_years$maxYear,animate = TRUE,
                round=TRUE, sep="",step=1),
    numericInput(NS(id, "myN"), strong("How Many:"), value=10,min=1,max= 50),
    radioButtons(NS(id, "myLev"), label=list(strong("Levels to show:"), actionButton(NS(id, "levelHelp"), label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                 choices=c("Top" = "lev1","Public Health" = "lev2","Detail" = "lev3"), inline=TRUE, selected = 'lev2')
  )
  
}

rankCause_ui_body <- function(id) {
  plotOutput(NS(id, "plot"))
}



rankCause_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
    shiny::observe({
      if (!is.null(urlParams) && length(urlParams) > 0) {
        
        if (urlParams$tab == id) { # Ensure the module ID matches the URL param
          if (NS(id, "myMeasure_sort") %in% names(urlParams)) shiny::updateRadioButtons(session, "myMeasure_sort", selected = urlParams[[NS(id, "myMeasure_sort")]])
          if (NS(id, "myMeanAge_sort") %in% names(urlParams)) shiny::updateRadioButtons(session, "myMeanAge_sort", selected = urlParams[[NS(id, "myMeanAge_sort")]])
          if (NS(id, "myYear") %in% names(urlParams)) shiny::updateSliderInput(session, "myYear", value = as.integer(urlParams[[NS(id, "myYear")]]))
          if (NS(id, "myN") %in% names(urlParams)) shiny::updateNumericInput(session, "myN", value = as.integer(urlParams[[NS(id, "myN")]]))
          if (NS(id, "myLev") %in% names(urlParams)) shiny::updateRadioButtons(session, "myLev", selected = urlParams[[NS(id, "myLev")]])
        }
      }
    })
    
    
    
    # Show/hide myMeanAge_sort based on myMeasure selection
    observe({
      
      if (input$myMeasure_sort == "mean.age") {
        shinyjs::show("myMeanAge_sort")
      } else {
        shinyjs::hide("myMeanAge_sort")
      }
      
    })
    
    # Render plot
    plot_step <- reactive({
      rankCause(myCounty = r_global_inputs$myLHJ, 
                myMeasure = input$myMeasure_sort, 
                myYear = input$myYear, 
                mySex = r_global_inputs$mySex, 
                myLev = input$myLev, 
                myN = input$myN, 
                myMeanAge_sort = input$myMeanAge_sort)
    })
    
    output$plot <- renderPlot(plot_step()$plotL)
    
  })
  
}