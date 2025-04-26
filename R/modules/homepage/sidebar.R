# Homepage sidebar ===========================================================

# Sidebar for home tab
appSidebar_home <- tagList(
  div(id = "sidebar_home",
      style = "padding: 15px",
      h3(style = "margin-bottom: 10px; color: #004c4c;", VERSION), 
      h4(style = "margin-bottom: 20px; color: #666;", format(Sys.Date(), "%b %d, %Y")),
      
      h4("Quick Links", style = "margin-bottom: 10px; color: #004c4c;"),
      tags$ul(style = "list-style-type: none; padding-left: 0;",
              tags$li(
                tags$a(href = "mailto:example@example.com", target = "_blank",
                       icon("envelope", style = "color: #004c4c; padding-right: 15px;"), 
                       " Email", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://example.com/report-bugs", target = "_blank",
                       icon("bug", style = "color: #004c4c; padding-right: 15px;"), 
                       " Report Bugs", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://example.com/feedback", target = "_blank",
                       icon("comments", style = "color: #004c4c; padding-right: 15px;"), 
                       " Share Feedback", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://github.com/mcSamuelDataSci/CACommunityBurden", target = "_blank",
                       icon("github", style = "color: #004c4c; padding-right: 15px;"), 
                       " Github", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://cdph.ca.gov", target = "_blank",
                       icon("building", style = "color: #004c4c; padding-right: 15px;"), 
                       " CDPH Site", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://www.cdph.ca.gov/Programs/OPP/Pages/OfficePolicyPlanning.aspx", target = "_blank",
                       icon("clipboard", style = "color: #004c4c; padding-right: 15px;"), 
                       " Office of Policy and Planning", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: 5px;"),
              tags$li(
                tags$a(href = "https://letsgethealthyca.org", target = "_blank",
                       icon("heartbeat", style = "color: #004c4c; padding-right: 15px;"), 
                       " Let's Get Healthy CA", style = "text-decoration: none; color: #333;")
              ),
              hr(style = "border: 1px solid #ddd; margin-top: 5px; margin-bottom: px;")
      )
  )
)