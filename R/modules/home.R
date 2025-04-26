# Homepage sidebar ===========================================================

# Sidebar for home tab
appSidebar_home <- tagList(
  div(id = "sidebar_home",
      style = "padding: 15px",
      h3(style = "margin-bottom: 10px; color: #0073e6;", constants$VERSION), 
      h4(style = "margin-bottom: 20px; color: #666;", format(Sys.Date(), "%b %d, %Y")),
      
      h4("Quick Links", style = "margin-bottom: 10px; color: #0073e6;"),
      tags$ul(style = "list-style-type: none; padding-left: 0;",
              tags$li(
                tags$a(href = "mailto:example@example.com", target = "_blank",
                       icon("envelope", style = "color: #0073e6; padding-right: 15px;"), 
                       " Email", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://example.com/report-bugs", target = "_blank",
                       icon("bug", style = "color: #0073e6; padding-right: 15px;"), 
                       " Report Bugs", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://example.com/feedback", target = "_blank",
                       icon("comments", style = "color: #0073e6; padding-right: 15px;"), 
                       " Share Feedback", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://github.com/mcSamuelDataSci/CACommunityBurden", target = "_blank",
                       icon("github", style = "color: #0073e6; padding-right: 15px;"), 
                       " Github", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://cdph.ca.gov", target = "_blank",
                       icon("building", style = "color: #0073e6; padding-right: 15px;"), 
                       " CDPH Site", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://www.cdph.ca.gov/Programs/OPP/Pages/OfficePolicyPlanning.aspx", target = "_blank",
                       icon("clipboard", style = "color: #0073e6; padding-right: 15px;"), 
                       " Office of Policy and Planning", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://letsgethealthyca.org", target = "_blank",
                       icon("heartbeat", style = "color: #0073e6; padding-right: 15px;"), 
                       " Let's Get Healthy CA", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: px;")
      )
      
      # h4("About the CCB", style = "margin-bottom: 10px; margin-top: 20px; color: #0073e6;"),
      # p(appText$app$textIntroA, style = "color: #555; line-height: 1.6;"),
      # p(appText$app$textIntroC, style = "color: #555; line-height: 1.6;"),
      # p(appText$app$textNote.real, style = "color: #555; line-height: 1.6;")
  )
)





# Anatomy Information -----------------------------------------------------------




## Anatomy Navigation Panel Set Up -------------------------------------------------
make_nav_panel <- function(myTitle, myValue, myID1, myID2, input1, input2, outputType = plotOutput) {
  nav_panel(title = myTitle, 
            value = myValue,
            layout_column_wrap(
              width = NULL,
              style = css(grid_template_columns = "1fr 2fr", padding = "20px"),
              htmlOutput(myID1, class = "geography-section"),
              fluidRow(id = "plot_preview_row", 
                       style = "border: 2px solid #7157db;",
                       layout_column_wrap(
                         width = NULL,
                         style = css(grid_template_columns = "1fr 1fr"),
                         gap = "0.1rem",
                         input1,
                         input2
                       ),
                       outputType(outputId = myID2, height = 600)
              )
              
            )
  )
}

## Anatomy menu information -------------------------------------------------------
anatomyMenuInfo <- function(selectedCategory) {
  
  # Filter data on selected menu category
  tDat <- homeAnatomyMenu %>% 
    filter(menuValue == selectedCategory)
  
  # Get menu description and start writing the HTML code to return
  tDescription <- tDat$description
  tHTML <- paste0("<div class='section-header'>", tDescription, "</div><ul class='geography-list'>")
  
  # Get itemized list and add to HTML code
  tList <- unlist(strsplit(tDat$infoList, ", "))
  tHighlight <- unlist(strsplit(tDat$highlight, ", "))
  
  for (tItem in tList) {
    tHTML <- paste0(tHTML, 
                    "<li class='geography-item'><span class='checkmark'>&#10004;</span> "
    )
    
    if (tItem %in% tHighlight) {
      tHTML <- paste0(tHTML, "<div class='highlighted-text'>", tItem, "</div></li>")
    } else {
      tHTML <- paste0(tHTML, tItem, "</li>")
    }
  }
  
  tHTML <- paste0(tHTML, "</ul>")
  return(HTML(tHTML))
  
}


# Homepage Information ---------------------------------------------------------

homeInfo <- read_docx("myInfo/appText/Homepage Information.docx")
homeInfo <- docx_extract_tbl(homeInfo, 1)

menuInfo <- function(selectedCategory) {
  homeInfo %>% 
    filter(menuValue == selectedCategory) %>% 
    pull(menuInfo) %>% 
    return()
}


# Featured assessments/stories -----------------------------------------------------------------------------------------------

featuredStory <- function(
    myURL,
    myImage,
    myTitle,
    myDescription
) {
  as_fill_carrier(
    a(href = myURL, target = "_blank", style = "text-decoration: none; color: unset;",
      card(
        card_body(img(src = paste0("Images/homepage/Featured Stories/", myImage)), padding = "20px", height = "50%"),
        card_body(h4(myTitle), p(myDescription), height = "50%")
      )
    )
  )
  
}

featuredStories <- list(
  feature1 = featuredStory(
    myURL = "https://skylab.cdph.ca.gov/communityBurden/SOPH/2024/Full%20Report.html",
    myImage = "sha_cm.png",
    myTitle = "State Health Assessment Core Module - 2024",
    myDescription = "This annual State Health Assessment (SHA) Core Module provides a snapshot of the health status for the entire California population and is used to identify key findings that contribute to informing the State Health Improvement Plan."
  ), 
  
  feature2 = featuredStory(
    myURL = "https://skylab.cdph.ca.gov/communityBurden/xMDA/2020_Excess_Mortality.html",
    myImage = "excess_mortality.png",
    myTitle = "2020/2021 Excess Mortality Data Brief",
    myDescription = "This Data Brief presents an analysis of excess mortality (increase in deaths) for California in 2020 and 2021, using California vital statistics death data (death certificates), and includes assessment of differential increases by race/ethnic group, age, and increases in deaths due to conditions other than COVID-19."
  ), 
  
  feature3 = featuredStory(
    myURL = "https://letsgethealthy.ca.gov/the-story/",
    myImage = "lghc.jpg",
    myTitle = "Let's Get Healthy California",
    myDescription = "Let’s Get Healthy California provides an ongoing statewide collaborative and systematic approach for assessing and monitoring the health status of California, identifying and prioritizing opportunities for health improvement, and promoting collective action towards comprehensive solutions that address the root causes of California’s toughest health challenges."
  ),
  
  feature4 = featuredStory(
    myURL = "https://www.cdph.ca.gov/Programs/OPP/Pages/State-of-Public-Health-Report.aspx",
    myImage = "sophr.jpg",
    myTitle = "State of Public Health Report 2024",
    myDescription = "The California State of Public Health Report is a biennial report established in the California Health and Safety Code (HSC 101320.3) and supported by the Future of Public Health investment​. The report uses multiple health measures and data sources to highlight the major trends and disparities in health outcomes across California while presenting opportunities, partnership, and collaboration to improve population health."
  )

)


# UI -------------------------------------------------------------------------

## Anatomy ---------------------------------

### Inputs -----

home_anatomy_inputs <- list(
  inputYear = function(id = NULL, label = NULL, choices, selected) {
    selectInput(id, label = label, choices = choices, selected = selected)
  }, 
  
  inputLHJ = function(id = NULL, label = NULL) {
    selectInput(id, label = label, choices = c("Alameda", "CALIFORNIA"), selected = "CALIFORNIA")
  }, 
  
  inputMeasure = function(id = NULL, label = NULL, choices, selected = "aRate") {
    selectInput(id, label = label, choices = choices, selected = selected)
  }
)


ui_anatomy <- function(id) {
  
  tagList(
    # Hero image section  ===============================================================================================
    div(class = "hero-wrapper",
        div(class = "hero-image",
            
            ## Header -----------------------------------------------------------------------------------------------
            div(class = "hero-text",
                h1("California Community Burden of Disease Engine"),
                p("Data visualization tool for health outcomes and risk factors.", style = "font-size:24px")
            ),
            
            # Using bslib: 
            card(
              full_screen = TRUE,
              class = "ccb_preview",
              card_header("CCB PREVIEW: Get a Sneak Peek at Our Powerful Visualization Tool"),
              navset_pill_list(
                id = "anatomy_menu_input",
                widths = c(3, 9),
                well = F,
                # make_nav_panel(myTitle, myValue, myID1, myID2, input1, input2, outputType = plotOutput)
                make_nav_panel(myTitle = "MAPS", myValue = "anatomy_mapsTab", myID1 = NS(id, "anatomy_info_mapsTab"), myID2 = NS(id, "anatomy_plot_maps"), 
                               input1 = home_anatomy_inputs$inputYear(id = NS(id, "myYear_maps"), label = NULL, choices = 2000:2021, selected = 2021), 
                               input2 = home_anatomy_inputs$inputMeasure(id = NS(id, "myMeasure_maps"), label = NULL, choices = choices$deathMeasuresAll, selected = "aRate"),  
                               outputType = leafletOutput),
                
                make_nav_panel(myTitle = "TRENDS", myValue = "anatomy_trendsTab", myID1 = NS(id, "anatomy_info_trendsTab"), myID2 = NS(id, "anatomy_plot_trends"), 
                               input1 = home_anatomy_inputs$inputLHJ(id = NS(id, "myLHJ_trends")), 
                               input2 = home_anatomy_inputs$inputMeasure(id = NS(id, "myMeasure_trends"), label = NULL, choices = choices$deathMeasuresAll, selected = "aRate"),  
                               outputType = plotOutput),
                
                make_nav_panel(myTitle = "RANK BY CAUSE", myValue = "anatomy_rankByCauseTab", myID1 = NS(id, "anatomy_info_rankByCauseTab"), myID2 = NS(id, "anatomy_plot_rankByCause"), 
                               input1 = home_anatomy_inputs$inputYear(id = NS(id, "myYear_rankByCause"), label = NULL, choices = 2000:2021, selected = 2021), 
                               input2 = home_anatomy_inputs$inputLHJ(id = NS(id, "myLHJ_rankByCause")),  
                               outputType = plotOutput),
                
                make_nav_panel(myTitle = "RANK BY GEOGRAPHY", myValue = "anatomy_rankByGeoTab", myID1 = NS(id, "anatomy_info_rankByGeographyTab"), myID2 = NS(id, "anatomy_plot_rankByGeo"), 
                               input1 = home_anatomy_inputs$inputYear(id = NS(id, "myYear_rankByGeo"), label = NULL, choices = 2000:2021, selected = 2021), 
                               input2 = home_anatomy_inputs$inputLHJ(id = NS(id, "myLHJ_rankByGeo")),  
                               outputType = highchartOutput)
              )
            ),
            
            layout_column_wrap(width = 1/2, 
                               as_fill_item(
                                 actionBttn(
                                   inputId = NS(id, "news_and_updates"),
                                   label = "News and Updates", 
                                   style = "material-flat",
                                   color = "danger", 
                                   icon = icon("code")
                                 )
                               ),
                               as_fill_item(
                                 actionBttn(
                                   inputId = NS(id, "newsletter"),
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






## Info --------------------------------------------------------------------------------

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
                    card_body(HTML(filter(homeInfo, menuValue == "aboutCCB")$menuInfo)
                              ))),
        
        ### Office of Policy and Planning ---------------------------------------------------
        nav_panel(title = "Office of Policy and Planning",
                  layout_column_wrap(
                    width = 1/2,
                    card_body(img(src = "Images/homepage/Info/opp.jpg")
                              ), 
                    card_body(HTML(filter(homeInfo, menuValue == "opp")$menuInfo) 
                              ))),
        
        ### Let's Get Healthy California ----------------------------------------------------
        nav_panel(title = "Let's Get Healthy California", 
                  layout_column_wrap(
                    width = 1/2, 
                    card_body(img(src = "Images/homepage/Info/lghc.jpg")
                              ), 
                    card_body(HTML(filter(homeInfo, menuValue == "lghc")$menuInfo)
                              ))), 
        
        ### SHA/SHIP ----------------------------------------------------
        nav_panel(title = "Let's Get Healthy California", 
                  layout_column_wrap(
                    width = 1/2, 
                    card_body(img(src = "https://www.scnsoft.com/blog-pictures/healthcare/health_data_analysis-01_1.png")
                              ), 
                    card_body(HTML(filter(homeInfo, menuValue == "lghc")$menuInfo)
                              )))
      )),
  br(), br()
)


## Featured Stories -------------------------------------------------------------------

ui_featured <- tagList(
  div(class = "featured-row", 
      h1("FEATURED POSTS"), 
      br(),
      layout_column_wrap(
        width = NULL,
        class = "featured-columns",
        gap = "2rem",
        featuredStories$feature1, featuredStories$feature2, featuredStories$feature3, featuredStories$feature4
      )
  )
)

## Homepage -------------------------------------------------------------------------------


home_ui_sidebar <- function(id) {
  appSidebar_home
}

home_ui_body <- function(id) {
  tagList(
    ui_anatomy(id),
    ui_info,
    ui_featured
  )  
}



home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Homepage =======================================================================================================================
    
    ## Anatomy - Plot Preview -------------------------------------
    output$anatomy_plot_maps <- renderLeaflet({
      plot_maps_step <- reactive({
        ccbMap(myMeasure = input$myMeasure_maps,  
               myYear = input$myYear_maps)
      })
      
      plot_maps_step()$tplot
    })
    
    output$anatomy_plot_trends <- renderPlot({
      plot_trends_step <- reactive({
        trendGeneric(myLHJ = input$myLHJ_trends,  
                     myMeasure = input$myMeasure_trends)
      })
      
      plot_trends_step()$plotL
    })
    
    
    output$anatomy_plot_rankByCause <- renderPlot({
      plot_rankByCause_step <- reactive({
        rankCause(myCounty = input$myLHJ_rankByCause,  
                  myYear = input$myYear_rankByCause)
      })
      
      plot_rankByCause_step()$plotL
    })
    
    output$anatomy_plot_rankByGeo <- renderHighchart({
      plot_rankByGeo_step <- reactive({
        rankGeo(myLHJ = input$myLHJ_rankByGeo,  
                myYear = input$myYear_rankByGeo)
      })
      
      plot_rankByGeo_step()$plotL_i
    })
    
    
    ## Anatomy - Bulleted list ------------------------------------
    
    # Define a list of tabs
    tabs <- c("mapsTab", "trendsTab", "rankByCauseTab", "rankByGeographyTab")
    
    # Use a loop to register renderUI calls
    for (tab in tabs) {
      local({
        tab_local <- tab
        output[[paste0("anatomy_info_", tab_local)]] <- renderUI({
          anatomyMenuInfo(tab_local)
        })
      })
    }
    
    # News and Updates --------------------------------------------
    
    # Show modal when button is clicked
    observeEvent(input$news_and_updates, {
      
      # Format updates as HTML
      news_html <- paste0(
        "<strong>", appText$updates$Date, ":</strong> ", appText$updates$Update, "<br><br>",
        collapse = ""
      )
      
      showModal(
        modalDialog(
          title = "News and Updates",
          div(
            style = "max-height: 400px; overflow-y: auto; padding-right: 10px;",
            HTML(news_html)
          ),
          easyClose = TRUE,
          size = "l"  # Large modal for better display
        )
      )
      
    })
    
    # Newsletter Subscription --------------------------------------
    
    observeEvent(input$newsletter, {
      showModal(
        modalDialog(
          title = "Subscribe to Our Newsletter",
          textInput(NS(id, "email_input"), "Enter your email:", placeholder = "example@domain.com"),
          actionButton(NS(id, "submit_email"), "Subscribe", class = "btn-primary"),
          easyClose = TRUE
        )
      )
    })
    
    observeEvent(input$submit_email, {
      req(input$email_input)  # Ensure email is entered
      
      # 🔹 Validate Email Format (Basic Check)
      if (!grepl("^\\S+@\\S+\\.\\S+$", input$email_input)) {
        showNotification("Invalid email format. Please enter a valid email.", type = "error")
        return()
      }
      
      # 🔹 Create a Data Frame
      new_entry <- data.frame(
        Email = input$email_input,
        Date = Sys.time(),
        stringsAsFactors = FALSE
      )
      
      # 🔹 Append Email to CSV File
      file_path <- "myInfo/newsletter_subscribers.csv"
      
      if (file.exists(file_path)) {
        existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
        updated_data <- rbind(existing_data, new_entry)
      } else {
        updated_data <- new_entry
      }
      
      write.csv(updated_data, file_path, row.names = FALSE)
      
      # 🔹 Show Confirmation
      showNotification("Thank you for subscribing!", type = "message")
      
      # 🔹 Close Modal
      removeModal()
    })
    
    
    
  })
  
}