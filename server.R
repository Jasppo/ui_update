server <- function(input, output, session) {
  
  # Keeping track of global inputs ==============================================
  # Global inputs defined in myFunctions/inputs/input_functions.R
  
  # Ensure shared inputs persist across tabs
  observeEvent(input$myCAUSE, { r_global_inputs$myCAUSE <- input$myCAUSE })
  observeEvent(input$myLHJ, { r_global_inputs$myLHJ <- input$myLHJ })
  observeEvent(input$myMeasure, { r_global_inputs$myMeasure <- input$myMeasure })
  observeEvent(input$mySex, { r_global_inputs$mySex <- input$mySex })
  
  # Initialize object to store current Tab/subtab
  currentTab <- reactiveValues()
  
  # Update tab/subtab value
  observe({
    currentTab$nav <- input$navsID
    tab_value <- paste0(input$navsID, "ID")
    currentTab$tab <- input[[tab_value]]
    print(paste0("Current Nav: ", currentTab$nav))
    print(paste0("Current Tab: ", currentTab$tab))
  })
  
  # Show/Hide Global Inputs based on tab ================================================================
  
  observe({
    if (currentTab$nav %in% c("home")) {
      hideAllInputs()
    } else {
      updateInputsOnTabId(currentTab$tab)
    }
  })
  
  # Update URL in real-time ========================================================
  
  # Extract relevant inputs for the active tab
  active_tab_inputs <- reactive({
    
    all_inputs <- reactiveValuesToList(input)  # Get all inputs (Shiny does NOT track this reactively)
    
    if (is.null(currentTab$tab) || !(currentTab$tab %in% names(TAB_INPUTS))) return(list())  # Avoid errors if tab is NULL
    
    # Track changes in inputs explicitly (makes Shiny reactive)
    tab_inputs_global <- isolate({
      TAB_INPUTS[[currentTab$tab]]
    })
    
    # Don't need to track tabInfo, supNote, download buttons
    tab_inputs_global <- tab_inputs_global[!tab_inputs_global %in% c("myTabInformation", "suppressionNote", "myDataDownload", "myChartDownload")]
    
    # Create a reactive dependency by referencing each input individually
    tab_inputs_global_values <- lapply(tab_inputs_global, function(x) input[[x]])  # Track global inputs
    
    # Filter only LOCAL inputs relevant to active tab (namespaced in Shiny modules)
    tab_inputs_local_keys <- names(all_inputs)[grepl(paste0("^", currentTab$tab, "-"), names(all_inputs))]  # Match exact tab inputs
    tab_inputs_local_keys <- tab_inputs_local_keys[!grepl("Help", tab_inputs_local_keys)]  # Remove help buttons
    tab_inputs_local_keys <- tab_inputs_local_keys[!grepl("plot_bounds|plot_zoom|plot_center", tab_inputs_local_keys)]  # Remove leaflet plot bounds, zoom, and center
    
    tab_inputs_local_values <- lapply(tab_inputs_local_keys, function(x) input[[x]])  # Track local inputs
    
    # Combine global and local inputs into a named list
    active_inputs <- c(
      setNames(tab_inputs_global_values, tab_inputs_global),
      setNames(tab_inputs_local_values, tab_inputs_local_keys)
    )
    
    return(active_inputs)
  })
  
  # Observe when active tab inputs change and update the URL
  observe({
    inputs <- active_tab_inputs()  # This now reacts when inputs change
    # print(inputs)  # Debugging - View inputs in console
    
    if (length(inputs) == 0) return()  # Avoid unnecessary updates
    
    # Convert all inputs to character and URL encode them
    query_params <- paste0(
      names(inputs), "=", sapply(inputs, function(x) URLencode(as.character(x))),
      collapse = "&"
    )
    
    # Construct the updated URL
    new_url <- paste0("?tab=", currentTab$tab, "&", query_params)
    
    # Update url
    updateQueryString(new_url, mode = "replace")
  })
  
  
  # Restore inputs from the URL when the app loads =====================================================
  
  
  # Parse URL to pass through each server module
  parseURL <- reactive({
    shiny::parseQueryString(session$clientData$url_search)
  })
  
  
  # Navigate to tab
  shiny::observe({
    query <- parseURL()
    # print(query)
    
    # Ensure tab exists in URL, default to "homeTab" if missing
    if (!is.null(query$tab) && query$tab %in% names(TAB_INPUTS)) {
      
      # Get nav and tab IDs
      navID <- links$ccbTab %>% filter(subtabID == query$tab) %>% pull(navID)
      tabID <- links$ccbTab %>% filter(subtabID == query$tab) %>% pull(tabID)
      
      # Update to tab, then subtab
      shiny::updateNavlistPanel(session, "navsID", selected = navID)
      shiny::updateNavlistPanel(session, tabID, selected = query$tab)
      
      
      # Restore Global Inputs (These appear across multiple tabs)
      if ("myLHJ" %in% names(query)) updateSelectInput(session, "myLHJ", selected = query$myLHJ)
      if ("myCAUSE" %in% names(query)) updateSelectInput(session, "myCAUSE", selected = query$myCAUSE)
      if ("myMeasure" %in% names(query)) updateSelectInput(session, "myMeasure", selected = query$myMeasure)
      if ("mySex" %in% names(query)) updateRadioButtons(session, "mySex", selected = query$mySex)
      
    }
    
  })
  
  # Call server modules =========================================================
  
  home_server("homeTab")
  
  maps_server("mapsTab", r_global_inputs, urlParams = parseURL())
  
  rankCause_server("rankByCauseTab", r_global_inputs, urlParams = parseURL())
  rankGeo_server("rankByGeoTab", r_global_inputs, urlParams = parseURL())
  
  trends_demo_server("demoTrendTab", r_global_inputs, urlParams = parseURL())
  trends_lifeExp_server("lifeExpTab", r_global_inputs, urlParams = parseURL())
  trendsTop_server("topTrendsTab", r_global_inputs, urlParams = parseURL())
  
  disparities_server("disparitiesTab", r_global_inputs, urlParams = parseURL())
  
  demographics_server("demographicsTab", r_global_inputs, urlParams = parseURL())
  
  
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