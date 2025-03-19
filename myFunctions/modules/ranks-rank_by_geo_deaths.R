# Ranks -> Rank by Geography

# 1. UI - Sidebar
# 2. UI - Main Panel (plot)
# 3. Server
#   - Condition #1:
#     - If cDeathRate or aRate selected for measure -> show Conf. Int. input
#     - Else Hide Conf. Int. Input
#   - Condition #2:
#     - If state selected -> show year slider
#     - Else hide year slider


rankGeo_ui_sidebar <- function(id) {
  
  tagList(
    sliderInput(NS(id, "myYear"), strong("Year:"), value=current_years$maxYear,min=2001,max=current_years$maxYear,animate = TRUE,
                round=TRUE, sep="",step=1),
    checkboxInput(NS(id, "myRefLine"),  strong("Reference Line"), value=FALSE),
    checkboxInput(NS(id, "myCI"), strong("95% CIs"), value=FALSE),
  )

}

rankGeo_ui_body <- function(id) {
    highchartOutput(NS(id, "plot"), height = "80vh")
}



rankGeo_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
    shiny::observe({
      if (!is.null(urlParams) && length(urlParams) > 0) {
        
        if (urlParams$tab == id) { # Ensure the module ID matches the URL param
          
          if (NS(id, "myYear") %in% names(urlParams)) shiny::updateSliderInput(session, "myYear", value = as.integer(urlParams[[NS(id, "myYear")]]))
          if (NS(id, "myRefLine") %in% names(urlParams)) shiny::updateCheckboxInput(session, "myRefLine", value = as.logical(urlParams[[NS(id, "myRefLine")]]))
          if (NS(id, "myCI") %in% names(urlParams)) shiny::updateCheckboxInput(session, "myCI", value = as.logical(urlParams[[NS(id, "myCI")]]))
        }
      }
    })

    
    # Show/hide myCI based on measure selection
    observe({
      if (r_global_inputs$myMeasure %in% c("cDeathRate", "aRate")) {
        shinyjs::show("myCI")
      } else {
        shinyjs::hide("myCI")
      }
    })

    # Show/hide myYear based on LHJ selection
    observe({
      if (r_global_inputs$myLHJ == constants$STATE) {
        shinyjs::show("myYear")
      } else {
        shinyjs::hide("myYear")
      }
    })
    
    # Render plot ====================================
    
    # Create a debounced version of the inputs
    debounced_inputs <- reactive({
      list(
        myLHJ = r_global_inputs$myLHJ, 
        myCause = r_global_inputs$myCAUSE, 
        myMeasure = r_global_inputs$myMeasure, 
        myYear = input$myYear,
        mySex = r_global_inputs$mySex, 
        myCI = input$myCI,
        myRefLine = input$myRefLine
      )
    }) %>% debounce(200) # 100 ms delay
    
    plot_step <- reactive({
      
      inputs <- debounced_inputs()
      
      rankGeo(myLHJ = inputs$myLHJ, 
              myCause = inputs$myCause, 
              myMeasure = inputs$myMeasure, 
              myYear = inputs$myYear,
              mySex = inputs$mySex, 
              myCI = inputs$myCI,
              myRefLine = inputs$myRefLine
      )
    })
    
    
    output$plot <- renderHighchart(plot_step()$plotL_i)
    
    
  })
  
}