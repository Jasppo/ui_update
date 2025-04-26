# Initialize
module_ui_body <- list()
module_server <- list()

# Home nav =======================================================
if (include_navs$homeID) {
  
  # Source dependencies
  source("R/modules/homepage/main.R")
  
  # Load body module
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("HOME"), value = "home", icon = icon("house"), home_ui_body("homeTab"))
    ))
  
  # Load server module
  module_server <- append(module_server, list(
    function(input, output, session, currentTab) home_server(input, output, session, currentTab)
  ))
  
}

# Maps nav =======================================================
if (include_navs$mapsID) {
  
  # Source dependencies
  source("R/plots/make_MAPS.R")
  source("R/modules/maps.R")
  
  # Load body module
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("MAPS"), value = "maps", icon = icon("map"),
              if (include_tabs$mapsTab) navset_pill(id = "mapsID", nav_panel(title = "MAP", value = "mapsTab", maps_ui_body("mapsTab")))
              )
    ))
  
  # Load server module
  module_server <- append(module_server, list(
    function(input, output, session, currentTab) maps_server(id = "mapsTab", input, output, session, currentTab)
  ))
  
}


# Ranks nav =======================================================
if (include_navs$ranksID) {
  
  # Initialize nav_panels (ui_body)
  ranks_tab_panels <- list()
  
  ## Rank By Cause tab ==================
  if (include_tabs$rankByCauseTab) {
    
    # Source dependencies
    source("R/plots/make_rank_CAUSE_chart.R")
    source("R/modules/ranks-rank_by_cause_deaths.R")
    
    # Load body module
    ranks_tab_panels <- append(ranks_tab_panels, list(
      nav_panel(title = "RANK BY CAUSE - DEATHS", value = "rankByCauseTab", rankCause_ui_body("rankByCauseTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) rankCause_server(id = "rankByCauseTab", input, output, session, currentTab)
    ))
    
  }
  
  
  ## Rank By Geo Tab ====================
  if (include_tabs$rankByGeoTab) {
    
    # Source dependencies
    source("R/plots/make_rank_GEOGRAPHY_chart.R")
    source("R/modules/ranks-rank_by_geo_deaths.R")
    
    # Load body module
    ranks_tab_panels <- append(ranks_tab_panels, list(
      nav_panel(title = "RANK BY GEOGRAPHY - DEATHS", value = "rankByGeoTab", rankGeo_ui_body("rankByGeoTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) rankGeo_server(id = "rankByGeoTab", input, output, session, currentTab)
    ))
    
  }
  
  # Append nav panels together ==========
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("RANKS"), value = "ranks", icon = icon("chart-simple"),
              navset_pill(id = "ranksID", !!!ranks_tab_panels)
              )
    ))
  
}


# Trends nav =======================================================
if (include_navs$trendsID) {
  
  # Initialize nav_panels (ui_body)
  trends_tab_panels <- list()
  
  ## Demo Trends tab ==================
  if (include_tabs$demoTrendTab) {
    
    # Source dependencies
    source("R/plots/make_ANY_TREND_chart.R")
    source("R/modules/trends-demo.R")
    
    # Load body module
    trends_tab_panels <- append(trends_tab_panels, list(
      nav_panel(title = "DEMOGRAPHIC TREND", value = "demoTrendTab", trends_demo_ui_body("demoTrendTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) trends_demo_server(id = "demoTrendTab", input, output, session, currentTab)
    ))
    
  }
  
  
  ## Life Expectancy Trends Tab ====================
  if (include_tabs$lifeExpTab) {
    
    # Source dependencies
    source("R/plots/make_LIFE-EXPECTANCY_chart.R")
    source("R/modules/trends-life_expectancy.R")
    
    # Load body module
    trends_tab_panels <- append(trends_tab_panels, list(
      nav_panel(title = "LIFE EXPECTANCY", value = "lifeExpTab", trends_lifeExp_ui_body("lifeExpTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) trends_lifeExp_server(id = "lifeExpTab", input, output, session, currentTab)
    ))
    
  }
  
  ## Top Trends Trends Tab ====================
  if (include_tabs$topTrendsTab) {
    
    # Source dependencies
    source("R/plots/make_topTrends.R")
    source("R/modules/trends-leading_causes.R")
    
    # Load body module
    trends_tab_panels <- append(trends_tab_panels, list(
      nav_panel(title = "LEADING CAUSES", value = "topTrendsTab", trendsTop_ui_body("topTrendsTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) trendsTop_server(id = "topTrendsTab", input, output, session, currentTab)
    ))
    
  }
  
  # Append nav panels together ==========
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("TRENDS"), value = "trends", icon = icon("chart-line"),
              navset_pill(id = "trendsID", !!!trends_tab_panels)
    )
  ))
  
}

# Disparities nav =======================================================
if (include_navs$disparitiesID) {
  
  # Initialize nav_panels (ui_body)
  disparities_tab_panels <- list()
  
  ## Disparities tab ==================
  if (include_tabs$disparitiesTab) {
    
    # Source dependencies
    source("R/plots/make_DISPARITY_chart.R")
    source("R/modules/disparities-disparities.R")
    
    # Load body module
    disparities_tab_panels <- append(disparities_tab_panels, list(
      nav_panel(title = "DISPARITIES", value = "disparitiesTab", disparities_ui_body("disparitiesTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) disparities_server(id = "disparitiesTab", input, output, session, currentTab)
    ))
    
  }
  
  # Append nav panels together ==========
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("DISPARITIES"), value = "disparities", icon = icon("scale-unbalanced"),
              navset_pill(id = "disparitiesID", !!!disparities_tab_panels)
    )
  ))
  
}

# Demographics nav =======================================================
if (include_navs$demographicsID) {
  
  # Initialize nav_panels (ui_body)
  demographics_tab_panels <- list()
  
  ## Disparities tab ==================
  if (include_tabs$demographicsTab) {
    
    # Source dependencies
    source("R/plots/make_DEMOGRAPHICS_chart.R")
    source("R/modules/demographics.R")
    
    # Load body module
    demographics_tab_panels <- append(demographics_tab_panels, list(
      nav_panel(title = "DEMOGRAPHICS", value = "demographicsTab", demographics_ui_body("demographicsTab"))
    ))
    
    # Load server module
    module_server <- append(module_server, list(
      function(input, output, session, currentTab) demographics_server(id = "demographicsTab", input, output, session, currentTab)
    ))
    
  }
  
  # Append nav panels together ==========
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("DEMOGRAPHICS"), value = "demographics", icon = icon("person"),
              navset_pill(id = "demographicsID", !!!demographics_tab_panels)
    )
  ))
  
}

# About nav =======================================================
if (include_navs$aboutID) {
  
  # Initialize nav_panels (ui_body)
  about_tab_panels <- list()
  
  ## Tech Notes tab ==================
  if (include_tabs$techNotesTab) {
    
    # Source dependencies
    source("R/modules/about-tech_notes.R")
    
    # Load body module
    about_tab_panels <- append(about_tab_panels, list(
      nav_panel(title = "TECHNICAL NOTES", value = "techNotesTab", techNotes_ui_body())
    ))
    
  }
  
  # Append nav panels together ==========
  module_ui_body <- append(module_ui_body, list(
    nav_panel(title = strong("ABOUT"), value = "about", icon = icon("person"),
              navset_pill(id = "aboutID", !!!about_tab_panels)
    )
  ))
  
}