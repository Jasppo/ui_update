maps_ui_sidebar <- function(id) {
  
  tagList(
    selectInput(NS(id, "myGeo"), strong("Geographic Level:"), choices=c("County","Community","Census Tract")),
    div(id = NS(id, "myGeoHelpText"), helpText(h6(appText$app$tractWarning,style="color:red; float:left; margin: 20px;"))),
    sliderInput(NS(id, "myYear"), strong("Year:"), 
                value = current_years$maxYear, min = 2001, max = current_years$maxYear, animate = TRUE, round = TRUE, sep = "", step = 1),  
    checkboxInput(NS(id, "myStateCut"), label=list(strong("State-based cutpoints"), actionButton("stateCutHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                  value=TRUE),
    radioButtons(NS(id, "myCutSystem"), label=list(strong("Cut-point method:"), actionButton("cutmethodHelp", label=inputStyles$helpIcon,style=inputStyles$myInputHelpButtonSty)),
                 choices=c("Quantile", "Fisher")), 
  )
  
}

maps_ui_body <- function(id) {
  leafletOutput(NS(id, "plot"), height = "80vh")
}



maps_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
    shiny::observe({
      if (!is.null(urlParams) && length(urlParams) > 0) {
        
        if (urlParams$tab == id) { # Ensure the module ID matches the URL param
          
          if (NS(id, "myGeo") %in% names(urlParams)) shiny::updateSelectInput(session, "myGeo", selected = urlParams[[NS(id, "myGeo")]])
          if (NS(id, "myYear") %in% names(urlParams)) shiny::updateSliderInput(session, "myYear", value = as.integer(urlParams[[NS(id, "myYear")]]))
          if (NS(id, "myStateCut") %in% names(urlParams)) shiny::updateCheckboxInput(session, "myStateCut", value = as.logical(urlParams[[NS(id, "myStateCut")]]))
          if (NS(id, "myCutSystem") %in% names(urlParams)) shiny::updateRadioButtons(session, "myCutSystem", selected = urlParams[[NS(id, "myCutSystem")]])
        }
      }
    })
    

    
    observe({
      if (r_global_inputs$myLHJ != constants$STATE & input$myGeo == "County") {
        updateSelectInput(inputId = "myGeo", selected = "Community")
      }
    })
    
    # Update myCause dropdown based on myGeo value
    observeEvent(input$myGeo, {

      if (input$myGeo == "County") {

        shinyjs::show("myYear")
        shinyjs::hide("myGeoHelpText")
        # updateSelectInput(inputId = "myCAUSE", choices = choices$cause_fullList, selected = r_global_inputs$myCAUSE)

      } else if (input$myGeo == "Community") {

        shinyjs::hide("myYear")
        shinyjs::hide("myGeoHelpText")


        # new_val <- ifelse(nchar(input$myCAUSE) == 4, substr(input$myCAUSE, 1, 3), input$myCAUSE)
        # rv_global$cause <- new_val
        # updateSelectInput(inputId = "myCAUSE", choices = choices$cause_phList, selected = r_global_inputs$myCAUSE)

      } else if (input$myGeo == "Census Tract") {

        shinyjs::hide("myYear")
        shinyjs::show("myGeoHelpText")

        # new_val <- ifelse(nchar(input$myCAUSE) >= 3, substr(input$myCAUSE, 1, 1), input$myCAUSE)
        # rv_global$cause <- new_val
        # updateSelectInput(inputId = "myCAUSE", choices = choices$cause_broadList, selected = r_global_inputs$myCAUSE)

      }

    })
    
    # Render map
    
    # Create a debounced version of the inputs
    debounced_inputs <- reactive({
      list(
        myLHJ = r_global_inputs$myLHJ, 
        myCause = r_global_inputs$myCAUSE, 
        myMeasure = r_global_inputs$myMeasure,  
        myYear = input$myYear,
        mySex  = r_global_inputs$mySex,   
        myStateCut = input$myStateCut,  
        myGeo = input$myGeo, 
        myCutSystem = input$myCutSystem
      )
    }) %>% debounce(300) # 100 ms delay
    
    plot_step <- reactive({
      
      inputs <- debounced_inputs()
      
      ccbMap(myLHJ = inputs$myLHJ, 
             myCause = inputs$myCause, 
             myMeasure = inputs$myMeasure,  
             myYear = inputs$myYear,
             mySex  = inputs$mySex,   
             myStateCut = inputs$myStateCut,  
             myGeo = inputs$myGeo, 
             myCutSystem = inputs$myCutSystem)
    })
    
    output$plot <- renderLeaflet(plot_step()$tplot)
    
    
  })
  
}