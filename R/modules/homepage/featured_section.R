# Function for creating UI elements for each featured story ==========================================

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

# Create list of featured stories =======================================================

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

# UI for Featured Stories section ========================================================

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