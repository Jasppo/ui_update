# Ranks -> Rank by Geography

# 1. UI - Sidebar
# 2. UI - Main Panel (plot)
# 3. Server

disparities_ui_sidebar <- function(id) {
  
  tagList(
    radioButtons(inputId = NS(id, "myYearGrouping"), label = strong("Years to Group:"), choices=c(1,3), inline = TRUE, selected = 1),
    radioButtons(NS(id, "myCompare"), 
                 list(label = strong("Compare to group with:"), actionButton(NS(id, "disparityCompareHelp"), label = inputStyles$helpIcon, style = inputStyles$myInputHelpButtonSty)), 
                 choices=c("Lowest Rate","Highest Rate"))
  )

}

disparities_ui_body <- function(id) {
  
  tagList(
    uiOutput(NS(id, "title")), 
    layout_columns(
      col_widths = c(8, 4, 12),
      row_heights = c(1, 1),
      
      card(full_screen = T, class = "bg-light", 
           card_header("Race/Ethnicity"), 
           card_body(highchartOutput(NS(id, "racePlot"), height = "35vh"))
           ),
      
      card(full_screen = T, class = "bg-light",
           card_header("Sex"), 
           card_body(highchartOutput(NS(id, "sexPlot"), height = "35vh"))
      ),
      
      card(full_screen = T, class = "bg-light",
           card_header("Age Group"), 
           card_body(highchartOutput(NS(id, "agePlot"), height = "35vh"))
      )
    )
  )
}



disparities_server <- function(id, r_global_inputs, urlParams = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Set up reactive value for myYearGrouping input
    year_val <- reactiveVal(1)
    
    year_title_val <- reactiveVal(as.character(current_years$currentYear))
    
    cause_title_val <- reactiveVal("All CAUSES")
    
    # Update reactive value when myYearGrouping changes
    observe({
      year_val(input$myYearGrouping)
      
      new_year_val <- ifelse(input$myYearGrouping == 1, as.character(current_years$currentYear), current_years$currentYearG3)
      year_title_val(new_year_val)
      
      new_cause_val <- links$deathCause %>% 
        filter(causeCode == r_global_inputs$myCAUSE) %>% 
        pull(causeName)
      cause_title_val(new_cause_val)
      
    })
    
    observe({
      
      if (r_global_inputs$myLHJ == constants$STATE) {
        updateRadioButtons(inputId = "myYearGrouping", choices = c(1, 3), selected = year_val())
      } else {
        updateRadioButtons(inputId = "myYearGrouping", choices = c(3), selected = 3)
      }
      
    })
    
    # Dynamic Titles =====================================
    output$title <- renderUI({
      h2(paste0("Disparities in Death Rates in ", cause_title_val(), " in ", r_global_inputs$myLHJ, ", ", year_title_val()))
    })
    
    
    # Render plot ====================================
    
    # Create a debounced version of the inputs
    debounced_inputs <- reactive({
      list(
        myLHJ = r_global_inputs$myLHJ,
        myCause = r_global_inputs$myCAUSE,
        myYearGrouping = input$myYearGrouping,
        myCompare = input$myCompare
      )
    }) %>% debounce(200) # 100 ms delay
    
    plot_step <- reactive({
      
      inputs <- debounced_inputs()
      
      disparity(myLHJ = inputs$myLHJ,
                myCause = inputs$myCause,
                myYearGrouping = inputs$myYearGrouping,
                myCompare = inputs$myCompare
      )
    })
  
    output$racePlot <- renderHighchart(plot_step()$plotL_race)
    output$sexPlot <- renderHighchart(plot_step()$plotL_sex)
    output$agePlot <- renderHighchart(plot_step()$plotL_age)
    
    
  })
  
}