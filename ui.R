ui <- tagList(
    shinyjs::useShinyjs(),
    tags$head(
      # Link the CSS file
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    ),
    page_navbar(
      
      theme = bs_theme(
        version = 5, # Ensure Bootstrap 5+ is available on your system
        `enable-gradients` = TRUE,
        `enable-shadows` = TRUE,
        `enable-rounded` = FALSE,
        `bslib-spacer` = "1rem",
        bg = "white", # Set background color globally
        fg = "#333333" # Set foreground globally
      ), 
      title = "CCB",
      window_title = "CCB",
      id = "navsID",
      inverse = TRUE,
      bg = "#004c4c",
      
      # Sidebar =================================================================================================
      
      sidebar = sidebar(
        id = "sidebar",
        width = 300,
        bg = "#FBFBFB",
        shinyjs::hidden(
          tagList(
            global_inputs$tabInformation,
            global_inputs$causeDropdown,
            global_inputs$lhjDropdown,
            global_inputs$measureDropdown,
            global_inputs$sexDropdown
          )
        ),
        
        ## Homepage tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'home'",
          home_ui_sidebar("homeTab")
        ),
        
        ## Maps tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'maps'",
          maps_ui_sidebar("mapsTab")
        ),
        
        
        ## Ranks tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'ranks' && input.ranksID === 'rankByCauseTab'",
          rankCause_ui_sidebar("rankByCauseTab")
        ), 
        
        conditionalPanel(
          "input.navsID === 'ranks' && input.ranksID === 'rankByGeoTab'",
          rankGeo_ui_sidebar("rankByGeoTab")
        ), 
        
        ## Trends tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'trends' && input.trendsID === 'demoTrendTab'",
          trends_demo_ui_sidebar("demoTrendTab")
        ),
        
        conditionalPanel(
          "input.navsID === 'trends' && input.trendsID === 'lifeExpTab'",
          trends_lifeExp_ui_sidebar("lifeExpTab")
        ),
        
        conditionalPanel(
          "input.navsID === 'trends' && input.trendsID === 'topTrendsTab'",
          trendsTop_ui_sidebar("topTrendsTab")
        ),
        
        ## Disparities tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'disparities' && input.disparitiesID === 'disparitiesTab'",
          disparities_ui_sidebar("disparitiesTab")
        ), 
        
        ## Demographics tab -------------------------------------------------------------------------------------------------
        
        conditionalPanel(
          "input.navsID === 'demographics' && input.demographicsID === 'demographicsTab'",
          demographics_ui_sidebar("demographicsTab")
        ), 
        
        # Bottom --------------------
        
        shinyjs::hidden(
          tagList(
              global_inputs$supNote,
              global_inputs$downloadData,
              global_inputs$downloadChart
              )
        )
  
      ),
      
      # Body =======================================================================================================
      
      ## Homepage tab -------------------------------------------------------------------------------------------------
      nav_panel(title = strong("HOME"), 
                value = "home",
                icon = icon("house"),
                home_ui_body("homeTab")
      ),
      
      ## Maps tab --------------------------------------------------------------------------------------------------
      nav_panel(title = strong("MAPS"), 
                value = "maps", 
                icon = icon("map"),
                navset_pill(id = "mapsID", 
                            nav_panel(title = "MAP", value = "mapsTab", maps_ui_body("mapsTab"))
                            )
      ),
      
      
      ## Ranks tab -------------------------------------------------------------------------------------------------
      nav_panel(title = strong("RANKS"),
                value = "ranks", 
                icon = icon("chart-simple"),
                navset_pill(id = "ranksID",
                           nav_panel(title = "RANK BY CAUSE - DEATHS", value = "rankByCauseTab", rankCause_ui_body("rankByCauseTab")), 
                           nav_panel(title = "RANK BY GEOGRAPHY - DEATHS", value = "rankByGeoTab", rankGeo_ui_body("rankByGeoTab"))
                )
      ), 
      
      ## Trends tab -------------------------------------------------------------------------------------------------
      nav_panel(title = strong("TRENDS"), 
                value = "trends",
                icon = icon("chart-line"),
                navset_pill(id = "trendsID",
                            nav_panel(title = "DEMOGRAPHIC TREND", value = "demoTrendTab", trends_demo_ui_body("demoTrendTab")),
                            nav_panel(title = "LIFE EXPECTANCY", value = "lifeExpTab", trends_lifeExp_ui_body("lifeExpTab")),
                            nav_panel(title = "LEADING CAUSES", value = "topTrendsTab", trendsTop_ui_body("topTrendsTab"))
                )
      ),
      
      ## Disparities tab -------------------------------------------------------------------------------------------------
      nav_panel(title = strong("DISPARITIES"),
                value = "disparities",
                icon = icon("scale-unbalanced"),
                navset_pill(id = "disparitiesID",
                           nav_panel(title = "DISPARITIES", value = "disparitiesTab", disparities_ui_body("disparitiesTab"))
                )
      ), 
      
      ## Demographics tab -------------------------------------------------------------------------------------------------
      nav_panel(title = strong("DEMOGRAPHICS"), 
                value = "demographics",
                icon = icon("person"),
                navset_pill(id = "demographicsID",
                            nav_panel(title = "DEMOGRAPHICS", value = "demographicsTab", demographics_ui_body("demographicsTab"))
                )
      ),
      
    )
  )
