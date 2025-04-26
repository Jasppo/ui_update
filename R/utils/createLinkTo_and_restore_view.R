generate_shareable_link <- function(input, currentTab, session) {
  
  # Get all input values
  all_inputs <- reactiveValuesToList(input)
  
  # Inputs defined for the current tab
  tab_inputs <- TAB_INPUTS[[currentTab$tab]]
  
  # Inputs to exclude
  exclude_inputs <- c(
    "myTabInformation", "copyLink", "suppressionNote", "myDataDownload", "myChartDownload"
  )
  
  # Also exclude by pattern match
  pattern_exclude <- c("Help", "plot_bounds", "plot_zoom", "plot_center")
  exclude_pattern_matches <- grep(paste(pattern_exclude, collapse = "|"), tab_inputs, value = TRUE)
  
  # Final list of inputs to track
  selected_input_ids <- setdiff(tab_inputs, c(exclude_inputs, exclude_pattern_matches))
  
  # Extract values safely
  selected_vals <- lapply(selected_input_ids, function(x) input[[x]])
  param_list <- setNames(
    lapply(selected_vals, function(x) if (is.null(x)) "" else x),
    selected_input_ids
  )
  
  # Encode
  encoded_params <- paste0(
    names(param_list), "=",
    sapply(param_list, function(x) URLencode(as.character(x), reserved = TRUE)),
    collapse = "&"
  )
  
  # Combine query
  query <- paste0("tab=", currentTab$tab, if (nchar(encoded_params) > 0) paste0("&", encoded_params) else "")
  
  # Full URL
  paste0(
    session$clientData$url_protocol,
    "//",
    session$clientData$url_hostname,
    session$clientData$url_pathname,
    "?",
    query
  )
}


# Restore view from URL parameters =========================================================

#' Restore global Shiny inputs from URL parameters
#'
#' @param session Shiny session object
#' @param input Shiny input object
#' @param TAB_INPUTS List of input IDs per tab (used for validating tabs)
#' @param links Object that includes ccbTab mapping (used to resolve tab ID and nav ID)
#'
#' @return An observer that restores inputs on app load
restore_inputs_from_url <- function(session, input, TAB_INPUTS, links) {
  
  parseURL <- reactive({
    shiny::parseQueryString(session$clientData$url_search)
  })
  
  observe({
    query <- parseURL()
    
    # 1. Navigate to the correct tab
    if (!is.null(query$tab) && query$tab %in% names(TAB_INPUTS)) {
      navID <- links$ccbTab %>% dplyr::filter(subtabID == query$tab) %>% dplyr::pull(navID)
      tabID <- links$ccbTab %>% dplyr::filter(subtabID == query$tab) %>% dplyr::pull(tabID)
      
      shiny::updateNavlistPanel(session, "navsID", selected = navID)
      shiny::updateNavlistPanel(session, tabID, selected = query$tab)
    }
    
    # 2. Try updating each input once
    for (param in names(query)) {
      if (param == "tab") next
      val <- query[[param]]
      
      # Try updateSelectInput
      err <- try(updateSelectInput(session, inputId = param, selected = val), silent = TRUE)
      if (!inherits(err, "try-error")) next
      
      # Try updateRadioButtons
      err <- try(updateRadioButtons(session, inputId = param, selected = val), silent = TRUE)
      if (!inherits(err, "try-error")) next
      
      # Try updateCheckboxInput
      err <- try(updateCheckboxInput(session, inputId = param, value = as.logical(val)), silent = TRUE)
      if (!inherits(err, "try-error")) next
      
      # Try updateSliderInput
      err <- try(updateSliderInput(session, inputId = param, value = as.numeric(val)), silent = TRUE)
      if (!inherits(err, "try-error")) next
      
      # Optional: Log or ignore
      message(paste("⚠️ Could not update input:", param))
    }
  })
}