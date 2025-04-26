demographics_ui_body <- function(id) {
   
  layout_columns(
    col_widths = c(6, 6, 6, 6),
    row_heights = c(1, 1),
    
    card(full_screen = T, class = "bg-light", 
         card_header(uiOutput("popDemo_title1")), 
         card_body(highchartOutput("popDemo_plot1", height = "35vh"))
    ),
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput("popDemo_title2")), 
         card_body(highchartOutput("popDemo_plot2", height = "35vh"))
    ),
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput("popDemo_title3")), 
         card_body(highchartOutput("popDemo_plot3", height = "35vh"))
    ), 
    
    card(full_screen = T, class = "bg-light",
         card_header(uiOutput("popDemo_title4")), 
         card_body(highchartOutput("popDemo_plot4", height = "35vh"))
    )
  )
}



demographics_server <- function(id, input, output, session, currentTab) {
  
  # Dynamic Titles =====================================
  output$popDemo_title1 <- renderUI({
    h2(paste0("Population by Race/Ethnicity in ", input$myLHJ, ", ", input$myYear))
  })
  
  output$popDemo_title2 <- renderUI({
    h2(paste0("Population Pyramid in ", input$myLHJ, ", ", input$myYear))
  })
  
  output$popDemo_title3 <- renderUI({
    h2(paste0("Population by Race/Ethnicity and Age Group in ", input$myLHJ, ", ", input$myYear))
  })
  
  output$popDemo_title4 <- renderUI({
    h2(paste0(input$myGroup, " Population Trend in ", input$myLHJ))
  })
  
  
  # Render plot ====================================
  plot1_step <- reactive({
    make_demoPop_RacePie(myCounty = input$myLHJ, 
                         myYear = input$myYear
    )
  })
  
  plot2_step <- reactive({
    make_demoPop_Pyramid(myCounty = input$myLHJ, 
                         myYear = input$myYear
    )
  })
  
  plot3_step <- reactive({
    make_demoPop_RaceAge(myCounty = input$myLHJ, 
                         myYear = input$myYear
    )
  })
  
  plot4_step <- reactive({
    make_demoPop_trend(myCounty = input$myLHJ, 
                       myGroup = input$myPopData_demoGroup
    )
  })
  
  output$popDemo_plot1 <- renderHighchart(plot1_step()$plotL)
  output$popDemo_plot2 <- renderHighchart(plot2_step()$plotL)
  output$popDemo_plot3 <- renderHighchart(plot3_step()$plotL)
  output$popDemo_plot4 <- renderHighchart(plot4_step()$plotL)
  
}
 