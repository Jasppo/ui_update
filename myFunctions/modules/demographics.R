demographics_ui_sidebar <- function(id) {
  
  tagList(
    sliderInput(NS(id, "myYear"), strong("Year:"), value=current_years$maxYear, min=2000, max=current_years$maxYear,animate = TRUE,
                round=TRUE,sep="",step=1), 
    selectInput(inputId = NS(id, "myGroup"), label = strong("In the bottom right chart, display trends by:"), 
                choices = c("Total", "Sex", "Race/Ethnicity", "Age Group"), selected = "Total")
  )

}

demographics_ui_body <- function(id) {
   
  layout_columns(
    col_widths = c(6, 6, 6, 6),
    row_heights = c(1, 1),
    
    card(full_screen = T, class = "bg-light", 
         card_header(uiOutput(NS(id, "title1"))), 
         card_body(highchartOutput(NS(id, "plot1"), height = "35vh"))
    ),
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput(NS(id, "title2"))), 
         card_body(highchartOutput(NS(id, "plot2"), height = "35vh"))
    ),
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput(NS(id, "title3"))), 
         card_body(highchartOutput(NS(id, "plot3"), height = "35vh"))
    ), 
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput(NS(id, "title4"))), 
         card_body(highchartOutput(NS(id, "plot4"), height = "35vh"))
    )
  )
}



demographics_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Dynamic Titles =====================================
    output$title1 <- renderUI({
      h2(paste0("Population by Race/Ethnicity in ", r_global_inputs$myLHJ, ", ", input$myYear))
    })
    
    output$title2 <- renderUI({
      h2(paste0("Population Pyramid in ", r_global_inputs$myLHJ, ", ", input$myYear))
    })
    
    output$title3 <- renderUI({
      h2(paste0("Population by Race/Ethnicity and Age Group in ", r_global_inputs$myLHJ, ", ", input$myYear))
    })
    
    output$title4 <- renderUI({
      h2(paste0(input$myGroup, " Population Trend in ", r_global_inputs$myLHJ))
    })
    
    
    # Render plot ====================================
    plot1_step <- reactive({
      make_demoPop_RacePie(myCounty = r_global_inputs$myLHJ, 
                           myYear = input$myYear
                           )
    })
    
    plot2_step <- reactive({
      make_demoPop_Pyramid(myCounty = r_global_inputs$myLHJ, 
                           myYear = input$myYear
      )
    })
    
    plot3_step <- reactive({
      make_demoPop_RaceAge(myCounty = r_global_inputs$myLHJ, 
                           myYear = input$myYear
      )
    })
    
    plot4_step <- reactive({
      make_demoPop_trend(myCounty = r_global_inputs$myLHJ, 
                         myGroup = input$myGroup
      )
    })
    
    output$plot1 <- renderHighchart(plot1_step()$plotL)
    output$plot2 <- renderHighchart(plot2_step()$plotL)
    output$plot3 <- renderHighchart(plot3_step()$plotL)
    output$plot4 <- renderHighchart(plot4_step()$plotL)
  })
  
}