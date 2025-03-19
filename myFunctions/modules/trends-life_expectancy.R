trends_lifeExp_ui_sidebar <- function(id) {
  
  raceList <- links$race %>% filter(!raceCode %in% c("Other", "Unknown")) %>% pull(raceNameShort)
  
  tagList(
    checkboxGroupButtons(inputId = NS(id, "mySexMult"), label = strong("Which Sex Groups?"),
                         choices = c("Total", "Male", "Female"),
                         selected = c("Male", "Female"),
                         individual=TRUE,size="sm"
    ), 
    checkboxGroupButtons(inputId = NS(id, "myRace"), label = strong("Which Race/Ethnic Groups?"), 
                         choices = raceList, 
                         selected = raceList[!raceList %in% c("Multi-Race","Total","NH/PI","AI/AN")],
                         individual = TRUE, size = "sm"
    ),
    radioButtons(inputId = NS(id, "myYearGrouping"), label = strong("Years to Group:"), choices=c(1,3,5), inline = TRUE, selected = 1),
    checkboxInput(inputId = NS(id, "myCI"), label = strong("95% CIs"), value = F)
  )
  
}

trends_lifeExp_ui_body <- function(id) {
  plotOutput(NS(id, "plot"), height = 700)
}



trends_lifeExp_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Render plot ====================================
    
    plot_step <- reactive({
      
      LEtrend(myLHJ = r_global_inputs$myLHJ, 
              mySexMult = input$mySexMult, 
              myRace = input$myRace, 
              myCI = input$myCI, 
              myYearGrouping = input$myYearGrouping
      )
    })
    
    
    output$plot <- renderPlot(plot_step()$plotL)
    
    
  })
  
}