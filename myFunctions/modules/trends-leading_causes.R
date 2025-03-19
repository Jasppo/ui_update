# Top Level:
# Hidden inputs:
# myBroadGroups, myN_topTrends, myYearRank

trendsTop_ui_sidebar <- function(id) {
  
  tagList(
    radioButtons(NS(id, "myLevShort"), label=list(strong("Levels to show:"), actionButton(NS(id, "levelShortHelp"), label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)),
                 choices=c("Top" = "lev1","Public Health" = "lev2"), inline=TRUE, selected = 'lev2'),
    checkboxGroupButtons(NS(id, "myBroadGroups"),
                         label = list(strong("Select one or more broad condition group:"), 
                                      actionButton(inputId= NS(id, "broadGroupHelp"), label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty_broadGroup)),
                         choices = c("All" = "0", 
                                     "Communicable" = "A", 
                                     "Cancer" = "B", 
                                     "Cardiovascular" = "C", 
                                     "Other Chronic" = "D", 
                                     "Injury" = "E", 
                                     "Perinatal" = "P"),
                         selected = c("A", "B", "C", "D", "E", "P"), 
                         individual=TRUE, size="sm"),
    numericInput(NS(id, "myN_topTrends"), strong("How many conditions:"), value=5,min=1,max= 50),
    checkboxInput(inputId = NS(id, "myLogTrans"), label = strong("Log Transform of Y Axis"), value=FALSE),
    sliderInput(NS(id, "myYearRank"), label = strong("Leading causes in which year?:"), value = current_years$maxYear, min = current_years$minYear, max = current_years$maxYear,
                round = TRUE, sep = "", step = 1),
    sliderInput(NS(id, "myYearRange"), label = strong("Year range To display:"), min = current_years$minYear, max = current_years$maxYear, 
                value = c(current_years$minYear, current_years$maxYear), sep = "", step = 1),
    actionButton(NS(id, "update_plot"), "Update Plot")
  )

}

trendsTop_ui_body <- function(id) {
  plotOutput(NS(id, "plot"), height = 700)
}

trendsTop_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Show/Hide Inputs based on myLev selection
    observeEvent(input$myLevShort, {
      
      if (input$myLevShort == "lev2") {
        
        shinyjs::show("myBroadGroups")
        shinyjs::show("myN_topTrends")
        shinyjs::show("myYearRank")
        
      } else {
        
        shinyjs::hide("myBroadGroups")
        shinyjs::hide("myN_topTrends")
        shinyjs::hide("myYearRank")
        
      }
      
    })
    
    # Render Plot
    
    plot_step <- reactive(topCauses_trends(
      myLHJ = r_global_inputs$myLHJ, 
      myMeasure = r_global_inputs$myMeasure, 
      myLogTrans = input$myLogTrans,
      myN = input$myN_topTrends,
      myLev = input$myLevShort, 
      myBroad = input$myBroadGroups, 
      myYearRange = input$myYearRange, 
      myYearRank = input$myYearRank
    )) %>% 
      bindCache(r_global_inputs$myLHJ, r_global_inputs$myMeasure, input$myLogTrans, input$myN_topTrends, input$myLevShort, 
                input$myBroadGroups, input$myYearRange, input$myYearRank) %>%
      bindEvent(input$update_plot, ignoreInit = F, ignoreNULL = F)
      
    
    output$plot <- renderPlot(plot_step()$plotL)
    
    
  })
  
}