trends_demo_ui_sidebar <- function(id) {
  
  tagList(
    selectInput(inputId = NS(id, "myDemoGroup"), label = strong("Display Trends By:"), 
                choices = c("Sex", "Age Group", "Race/Ethnicity", "Education"), selected = "Sex"),
    selectInput(NS(id, "myMeasure"),  label=list(strong("Measure:"), actionButton("measureHelp", label=inputStyles$helpIcon,style=inputStyles$myInputHelpButtonSty)),
                choices = choices$deathMeasuresAll, selected = "aRate"),
    radioButtons(inputId = NS(id, "myYearGrouping"), label = strong("Years to Group:"), choices=c(1,3,5), inline = TRUE, selected = 1),
    checkboxInput(inputId = NS(id, "myLogTrans"), label = strong("Log Transform of Y Axis"), value=FALSE),
    checkboxInput(inputId = NS(id, "myMultiRace"), label = strong("Include Multirace Line"), value=FALSE),
    div(id = NS(id, "myMultiRaceNote"), helpText(h6("** Note: Multirace data are NOT RELIABLE due to changing data collection practices", style="color:red; float:left; margin: 20px;")))
  )
  
}

trends_demo_ui_body <- function(id) {
  plotOutput(NS(id, "plot"))
}

trends_demo_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    # Update inputs based on URL if custom URL is supplied ------------------------------------------------------------------------------
    shiny::observe({
      if (!is.null(urlParams) && length(urlParams) > 0) {
        
        if (urlParams$tab == id) { # Ensure the module ID matches the URL param
          if (NS(id, "myDemoGroup") %in% names(urlParams)) shiny::updateSelectInput(session, "myDemoGroup", selected = urlParams[[NS(id, "myDemoGroup")]])
          if (NS(id, "myMeasure") %in% names(urlParams)) shiny::updateSelectInput(session, "myMeasure", selected = urlParams[[NS(id, "myMeasure")]])
          if (NS(id, "myYearGrouping") %in% names(urlParams)) shiny::updateRadioButtons(session, "myYearGrouping", selected = as.integer(urlParams[[NS(id, "myYearGrouping")]]))
          if (NS(id, "myLogTrans") %in% names(urlParams)) shiny::updateCheckboxInput(session, "myLogTrans", value = as.logical(urlParams[[NS(id, "myLogTrans")]]))
          if (NS(id, "myMultiRace") %in% names(urlParams)) shiny::updateCheckboxInput(session, "myMultiRace", value = as.logical(urlParams[[NS(id, "myMultiRace")]]))
        }
      }
    })
    
    
    # Store Year as a Reactive Value
    rv <- reactiveValues(measure = "aRate", year = 1)
    
    # Update measure reactive value whenever the measure input changes
    observeEvent(input$myMeasure, {
      rv$measure <- input$myMeasure
    })
    
    # Update year reactive value whenever the year input changes
    observeEvent(input$myYearGrouping, {
      rv$year <- input$myYearGrouping
    })
    
    observe({
      
      if (input$myDemoGroup %in% c("Sex", "Age Group", "Race/Ethnicity")) {
        
        shinyjs::show(id = "myYearGrouping")
        shinyjs::show(id = "myLogTrans")
        
        if (input$myDemoGroup == "Sex") {
          
          shinyjs::hide(id = "myMultiRace")
          shinyjs::hide(id = "myMultiRaceNote")
          updateSelectInput(inputId = "myMeasure", choices = choices$deathMeasures_noSMR, selected = rv$measure)
          updateRadioButtons(inputId = "myYearGrouping", choices=c(1,3,5), inline=TRUE, selected = rv$year)
          
          
        } else if (input$myDemoGroup == "Age Group") {
          
          shinyjs::hide(id = "myMultiRace")
          shinyjs::hide(id = "myMultiRaceNote")
          rv$measure <- case_when(input$myMeasure == "aRate" ~ "cDeathRate", 
                                  input$myMeasure == "YLL.adj.rate" ~ "YLLper",
                                  TRUE ~ input$myMeasure
                                  )
          
          updateSelectInput(inputId = "myMeasure", choices = choices$deathMeasures_ageTrend, selected = rv$measure)
          
          if (r_global_inputs$myLHJ == constants$STATE) {
            
            rv$year <- ifelse(input$myYearGrouping == 5, 1, input$myYearGrouping)
            updateRadioButtons(inputId = "myYearGrouping", choices = c(1,3), inline=TRUE, selected = rv$year)
            
          } else {
            
            rv$year <- 3 # Update year reactive value
            updateRadioButtons(inputId = "myYearGrouping", choices=c(3), inline=TRUE, selected = 3)
            
          }
          
          
        } else if (input$myDemoGroup == "Race/Ethnicity") {
          
          shinyjs::show(id = "myMultiRace")
          if (input$myMultiRace) {
            shinyjs::show(id = "myMultiRaceNote")
          } else {
            shinyjs::hide(id = "myMultiRaceNote")
          }
          
          updateSelectInput(inputId = "myMeasure", choices = choices$deathMeasures_noSMR, selected = rv$measure)
          
          if (r_global_inputs$myLHJ == constants$STATE) {
            
            rv$year <- ifelse(input$myYearGrouping == 5, 1, input$myYearGrouping)
            updateRadioButtons(inputId = "myYearGrouping", choices=c(1,3), inline=TRUE, selected = rv$year)
            
          } else {
            
            rv$year <- 3 # Update year reactive value
            updateRadioButtons(inputId = "myYearGrouping", choices=c(3), inline=TRUE, selected = 3)
            
          }
          
        }
        
      } else if (input$myDemoGroup == "Education") {
        
        shinyjs::hide(id = "myYearGrouping")
        shinyjs::hide(id = "myLogTrans")
        shinyjs::hide(id = "myMultiRace")
        shinyjs::hide(id = "myMultiRaceHelpText")
        
        updateSelectInput(inputId = "myMeasure", choices = choices$deathMeasures_noSMR, selected = rv$measure)
        
        
      }
      
    })
    
    # Render plot ====================================
    
    # Create a debounced version of the inputs
    debounced_inputs <- reactive({
      list(
        myDemoGroup = input$myDemoGroup,
        myLHJ = r_global_inputs$myLHJ,
        myCause = r_global_inputs$myCAUSE,
        myMeasure = input$myMeasure,
        mySex = "Total",
        myYearGrouping = input$myYearGrouping,
        myLogTrans = input$myLogTrans
      )
    }) %>% debounce(200) # 100 ms delay
    
    plot_step <- reactive({
      
      inputs <- debounced_inputs()
      
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
    

    output$plot <- renderPlot(plot_step()$plotL)
    
    
  })

}