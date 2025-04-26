# Define Tabs to Highlight ====================================

homepage_highlights <- list(
  list(
    id = "mapsTab",
    nav = "maps",
    title = "Maps",
    subtitle = "Explore health outcomes by county, community, or census tract",
    icon = "map"
  ),
  list(
    id = "rankByGeoTab",
    nav = "ranks",
    title = "Rank by Geography",
    subtitle = "See which regions have the highest mortality rates",
    icon = "chart-bar"
  ),
  list(
    id = "rankByCauseTab",
    nav = "ranks",
    title = "Rank by Cause",
    subtitle = "See the leading causes of death across regions",
    icon = "chart-bar"
  ),
  list(
    id = "demoTrendTab",
    nav = "trends",
    title = "Demographic Trends",
    subtitle = "Analyze trends by age, sex, or race/ethnicity over time",
    icon = "chart-line"
  ),
  list(
    id = "lifeExpTab",
    nav = "trends",
    title = "Life Expectancy",
    subtitle = "Analyze life expectancy trends by sex and/or race/ethnicity",
    icon = "chart-line"
  ),
  list(
    id = "topTrendsTab",
    nav = "trends",
    title = "Leading Causes",
    subtitle = "Analyze trends of leading causes of death",
    icon = "chart-line"
  ),
  list(
    id = "disparitiesTab",
    nav = "disparities",
    title = "Disparities",
    subtitle = "Visualize inequalities by social or demographic groups",
    icon = "scale-unbalanced"
  ), 
  list(
    id = "demographicsTab",
    nav = "demographics",
    title = "Population",
    subtitle = "Understand the makeup of your community",
    icon = "person"
  ),
  list(
    id = "techNotesTab",
    nav = "about",
    title = "Technical Documentation",
    subtitle = "View technical notes",
    icon = "note-sticky"
  )
)

# Render Home Cards =====================================

render_home_cards <- function() {
  layout_columns(
    width = 3,
    gap = "2rem",
    !!!lapply(homepage_highlights, function(h) {
      card(
        class = "card-hover",
        style = "border-radius: 16px; background: linear-gradient(135deg, #f7f9fa, #e9f5f2);",
        full_screen = FALSE,
        card_body(
          div(
            class = "text-center",
            icon(h$icon, class = "card-icon"),
            div(class = "card-header-title", h$title),
            div(class = "card-subtext mb-3", h$subtitle)
          ),
          div(
            class = "d-flex justify-content-center gap-2 mt-2",
            actionButton(paste0("more_", h$id), "Learn More", icon = icon("info-circle"), class = "btn-sm btn-outline-primary"),
            actionButton(paste0("go_", h$id), "Go", icon = icon("arrow-right"), class = "btn-sm btn-primary")
          )
        )
      )
    })
  )
}

# UI for hero section ===========================================================

ui_hero <- function() {
  
  tagList(
    # Hero image section
    div(class = "hero-wrapper",
        div(class = "hero-image",
            
            ## Header
            div(class = "hero-text",
                h1("California Community Burden of Disease Engine"),
                p("Data visualization tool for health outcomes and risk factors.")
            ),
            
            render_home_cards(),
            br(), br(),
            
            # News/Updates and Newletter subscription buttons
            layout_column_wrap(width = 1/2, 
                               as_fill_item(
                                 actionBttn(
                                   inputId = "news_and_updates",
                                   label = "News and Updates", 
                                   style = "material-flat",
                                   color = "danger", 
                                   icon = icon("code")
                                 )
                               ),
                               as_fill_item(
                                 actionBttn(
                                   inputId = "newsletter",
                                   label = "Subscribe to our Newsletter", 
                                   style = "material-flat",
                                   color = "danger",
                                   icon = icon("envelope")
                                 ))
            )
            
            
        )),
    br(), br()
  )
  
  
}


# Server logic ================================================================

handle_homepage_cards <- function(input, output, session) {
  
  lapply(homepage_highlights, function(h) {
    
    observeEvent(input[[paste0("more_", h$id)]], {
      showModal(
        modalDialog(
          title = h$title,
          HTML(paste0("<p>", h$subtitle, "</p><p><em>[More details or illustrations here]</em></p>")),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
    
    observeEvent(input[[paste0("go_", h$id)]], {
      updateNavlistPanel(session, "navsID", selected = h$nav)
      updateNavlistPanel(session, paste0(h$nav, "ID"), selected = h$id)
    })
  })
}


