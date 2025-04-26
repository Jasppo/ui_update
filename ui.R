ui <- tagList(
    shinyjs::useShinyjs(),
    tags$head(
      # Link the CSS file
      tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
    ),
    tags$head(
      # JS file - For TOC navigation in tech doc tab
      tags$script(src = "js/tech-doc-nav.js")
    ),
    page_navbar(
      theme = bs_theme(
        version = 5, # Ensure Bootstrap 5+ is available on your system
        `enable-gradients` = TRUE,
        `enable-shadows` = TRUE,
        `enable-rounded` = FALSE,
        `bslib-spacer` = "1rem",
        bg = app_colors$bg, # Set background color globally
        fg = app_colors$fg, # Set foreground globally
        primary = app_colors$primary,
        secondary = app_colors$secondary,
        success = app_colors$success,
        info = app_colors$info,
        warning = app_colors$warning,
        danger = app_colors$danger
      ), 
      title = "CCB",
      window_title = "CCB",
      id = "navsID",
      navbar_options = navbar_options(theme = "dark", bg = "#004c4c", fg = "#333"),

      # Sidebar =================================================================================================
      sidebar = sidebar(
        id = "sidebar",
        width = 300,
        bg = "#FBFBFB",
        shinyjs::hidden(
          tagList(home_ui_sidebar(), 
                  techNotes_ui_sidebar(),
                  do.call(tagList, input_widgets)  # input_widgets Created in R/input_widgets.R
                  )
          ) 
      ), 
      
      # Body =======================================================================================================
      !!!module_ui_body # Created in R/load_modules_and_plots.R
    )
)