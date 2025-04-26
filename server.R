server <- function(input, output, session) {
  
  # Initialize object to store current Tab/subtab
  currentTab <- reactiveValues(nav = NULL, 
                               tab = NULL)
  
  # Update tab/subtab value
  observe({
    currentTab$nav <- input$navsID
    tab_value <- paste0(input$navsID, "ID")
    currentTab$tab <- input[[tab_value]]
    print(paste0("Current Nav: ", currentTab$nav))
    print(paste0("Current Tab: ", currentTab$tab))
  })
  
  # Show/hide inputs in sidebar
  observe({
    if (currentTab$nav == "home") {
      hideAllInputs() # Function created in R/input_widgets.R
      shinyjs::hide("sidebar_tech_notes")
      shinyjs::show("sidebar_home")
    } else if (currentTab$nav == "about" & currentTab$tab == "techNotesTab") {
      hideAllInputs() # Function created in R/input_widgets.R
      shinyjs::hide("sidebar_home")
      shinyjs::show("sidebar_tech_notes")
    } else {
      shinyjs::hide("sidebar_home")
      shinyjs::hide("sidebar_tech_notes")
      showTabInputs(currentTab$tab) # Function created in R/input_widgets.R
      }
  })
  
  

  # Generate link to view ========================================================
  
  observeEvent(input$copyLink, {
    req(currentTab$tab)
    url <- generate_shareable_link(input, currentTab, session) # Utility function
    
    showModal(modalDialog(
      title = "Copy This Link",
      textInput("copyField", label = NULL, value = url, width = "100%"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Restore State from URL on Load =============================================
  restore_inputs_from_url(session, input, TAB_INPUTS, links) # Utility function

  # Call server modules =========================================================
  
  lapply(module_server, function(f) f(input, output, session, currentTab)) # module_server created in R/load_modules_and_plots.R
  
  # Tab Information ================================================================
  
  # Show modal when button is clicked
  observeEvent(input$myTabInformation, {
    
    tabInfo <- appText$tabInfo %>% 
      filter(subTabID == currentTab$tab) %>% 
      pull(Text)
    
    if (length(tabInfo) == 0) {
      tabInfo <- "Help Info is not yet available for this tab."
    } else {
      tabInfo <- HTML(tabInfo)
    }
    
    showModal(
      modalDialog(
        title = "About this tab",
        div(
          style = "max-height: 400px; overflow-y: auto; padding-right: 10px;",
          tabInfo
        ),
        easyClose = TRUE,
        size = "l"  # Large modal for better display
      )
    )
    
  })
  
  
  

}