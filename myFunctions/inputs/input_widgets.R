# Help Styles for Inputs =============================================

inputStyles <- list(
  myInputHelpButtonSty = "width:20px;color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;margin:0px;margin-left:10px;float:right;",
  helpIcon = "?", 
  myInputHelpButtonSty_broadGroup = "width:20px;  color:#fff; background-color:#337ab7; border-color:white; padding:0px; font-size: 18px;margin:0px;margin-left:10px;display:inline-block;"
)



# Global Input Widgets ================================================

# - Tab Information
# - Download Data
# - Download Chart
# - Suppression Note
# - Cause of death dropdown
# - LHJ Dropdown
# - Measure Dropdown
# - Sex Dropdown

dottedSelectInput <- function(inputId, label, choices, height = "500px") {
  list(
    tags$style(HTML(paste0(
      sep = "\n",
      # Jaspreet: this only changes the myCAUSE dropdown 
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y { ",
      "width: auto !important;",
      "min-width: 25% !important;", # set to 25% because this is now a child of the body
      " overflow: auto;", # Jaspreet: provides a scrollbar to dropdown menus if needed
      "white-space: nowrap;",
      "height: 350px;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown > ul {",
      "  margin: 0 !important;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown [data-selectable] {",
      "  padding: 5px 15px !important;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content {",
      "  height: 100% !important;",
      "  max-height: 100% !important",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul {",
      "  padding-left: 15px;",
      "  margin-bottom: 0;",
      "  list-style: none;",
      "  line-height: 18px;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul {",                   # Level 0 font size, color, weight
      "  font-size: 18px;",                              
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul {",              # Level 1 font size, color, weight
      "  font-size: 16px;",
      "  font-weight: bold;",
      "  color: rgb(23,78,134);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul > ul {",          # Level 2 font size, color, weight 
      "  font-size: 14px;",
      "  font-weight: bold;",
      "  color: rgb(0,0,0);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > ul > ul > ul > ul {",     # Level 3 font size, color, weight  
      "  font-size: 14px;",
      "  font-weight: normal;",
      "  color: rgb(23,78,134);",
      "  list-style: none;",
      "}",
      ".selectize-dropdown.single.form-control.plugin-selectize-plugin-a11y .selectize-dropdown-content > li {",
      "  font-size: inherit;",
      "  color: inherit;",
      "}"
    ))),
    selectizeInput(
      inputId = inputId,
      label = label,
      choices = choices,
      options = list(
        maxOptions = length(choices),
        dropdownParent = 'body', # Jaspreet - Avoid the clipping issue
        render = I(
          paste(
            collapse = " ", sep = " ",
            "{ option: function(item, escape) { ",
            "if (item.label.substring(0, 1) !== '.') {",
            "return '<div>' + item.label + '</div>';",
            "}",
            "var dots = item.label.match(/^[.]+/)[0];",
            "var text = item.label.replace(/^[.]+/, '');",
            "var open = dots.replace(/[.]/g, '<ul>');",
            "var close = dots.replace(/[.]/g, '</ul>');",
            "return open + '<li><div>' + text + '</div></li>' + close;",
            "}, ",
            "item: function(item, escape) { ",
            "return '<div>' + item.label.replace(/^[.]+/, '') + '</div>';",
            "}" ,
            "}"
          )
        )
      ), 
      tags$head(tags$style(".selectize-control.single { width: 400px; z-index: 1; }"))
    )
  )
}



global_inputs <- list(
  tabInformation = actionButton(inputId = "myTabInformation", label = "Show Tab Information", style = "width: 100%;"), 
  downloadData = downloadButton(outputId = "myDataDownload", label = "Data"),
  downloadChart = downloadButton(outputId = "myChartDownload", label = "Chart"), 
  supNote = div(id="suppressionNote",
                paste('Note: All measures associated with counts <', "11",', as well as necessary complementrary counts/measures are excluded for data de-identification purposes'),style="color:blue;font-size:12px;padding-left:5px;"
                ), 
  causeDropdown = dottedSelectInput("myCAUSE", 
                                    label=list(strong("Cause of Death:"), actionButton(inputId="causeHelp", label=inputStyles$helpIcon, style=inputStyles$myInputHelpButtonSty)), 
                                    choices=choices$cause_fullList), 
  lhjDropdown = selectInput("myLHJ",strong("County/State:"), choices = choices$counties, selected = constants$STATE), 
  measureDropdown = selectInput("myMeasure",  label=list(strong("Measure:"), actionButton("measureHelp", label=inputStyles$helpIcon,style=inputStyles$myInputHelpButtonSty)),
                                choices = choices$deathMeasuresAll, selected = "aRate"), 
  sexDropdown = radioButtons("mySex", strong("Sex:"), choices=c("Total","Female","Male"), inline=TRUE)
)