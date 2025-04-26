# UI for Info section ===========================================================

ui_info <- tagList(
  div(class = "info-row",
      h1("WHAT IS THE CCB?"),
      br(),
      
      navset_card_pill(
        placement = "above",
        
        ### About the CCB --------------------------------------------------------------------
        nav_panel(title = "About the CCB",
                  layout_column_wrap(
                    width = 1/2,
                    card_body(img(src = "Images/homepage/Info/CCB.png")
                    ), 
                    card_body(HTML(filter(appText$homeInfo, menuValue == "aboutCCB")$menuInfo)
                    ))),
        
        ### Office of Policy and Planning ---------------------------------------------------
        nav_panel(title = "Office of Policy and Planning",
                  layout_column_wrap(
                    width = 1/2,
                    card_body(img(src = "Images/homepage/Info/opp.jpg")
                    ), 
                    card_body(HTML(filter(appText$homeInfo, menuValue == "opp")$menuInfo) 
                    ))),
        
        ### Let's Get Healthy California ----------------------------------------------------
        nav_panel(title = "Let's Get Healthy California", 
                  layout_column_wrap(
                    width = 1/2, 
                    card_body(img(src = "Images/homepage/Info/lghc.jpg")
                    ), 
                    card_body(HTML(filter(appText$homeInfo, menuValue == "lghc")$menuInfo)
                    ))), 
        
        ### SHA/SHIP ----------------------------------------------------
        nav_panel(title = "Let's Get Healthy California", 
                  layout_column_wrap(
                    width = 1/2, 
                    card_body(img(src = "https://www.scnsoft.com/blog-pictures/healthcare/health_data_analysis-01_1.png")
                    ), 
                    card_body(HTML(filter(appText$homeInfo, menuValue == "lghc")$menuInfo)
                    )))
      )),
  br(), br()
)