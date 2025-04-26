# Source files =================================================================

source("R/modules/homepage/sidebar.R")
source("R/modules/homepage/hero_section.R")
source("R/modules/homepage/info_section.R")
source("R/modules/homepage/featured_section.R")

# Homepage ui sidebar ==========================================================

home_ui_sidebar <- function() {
  appSidebar_home
}

# Homepage ui body ==============================================================

home_ui_body <- function(id) {
  tagList(
    ui_hero(),
    ui_info,
    ui_featured
  )  
}



# Homepage server ==============================================================

home_server <- function(input, output, session, currentTab) {
  
  ## Hero section =======================
  
  handle_homepage_cards(input, output, session)
  
  
  ## News and Updates ===================
  
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
  
  ## Newsletter Subscription =====================
  
  observeEvent(input$newsletter, {
    showModal(
      modalDialog(
        title = "Subscribe to Our Newsletter",
        textInput("email_input", "Enter your email:", placeholder = "example@domain.com"),
        actionButton("submit_email", "Subscribe", class = "btn-primary"),
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
  
  
}