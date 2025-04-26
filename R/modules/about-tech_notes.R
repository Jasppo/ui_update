techNotes_ui_sidebar <- function() {
  
  tagList(
    div(
      id = "sidebar_tech_notes",
      HTML(
        '<nav class="tech-doc-nav">
        <ol>
          <li><a href="#notes">Notes</a>
            <ul>
              <li class=""><a href="#notes--data-deidentification">Data De-Identification</a></li>
              <li class=""><a href="#notes--year-group">Year or Year Group</a></li>
              <li class=""><a href="#notes--race-and-ethnicity-death">Race and Ethnicity for Death Data</a></li>
              <li class=""><a href="#notes--race-and-ethnicity-hospitalization-ed">Race and Ethnicity for Hospitalization and Emergency Department Data</a></li>
              <li class=""><a href="#notes--communities">Communities</a></li>
              <li class=""><a href="#notes--LHJ">County, City, and Local Health Jurisdiction (LHJ) Designation</a></li>
            </ul>
          </li>
          <li><a href="#data-sources">Data Sources</a>
            <ul>
              <li class=""><a href="#data-sources--death-data">Death</a></li>
              <li class=""><a href="#data-sources--hospitalization-and-ed-data">Hosp & ED</a></li>
              <li class=""><a href="#data-sources--population-data">Population</a></li>
              <li class=""><a href="#data-sources--sdoh">SDOH</a></li>
            </ul>
          </li>
          <li><a href="#icd-10">Condition List and ICD-10 Mapping for Deaths</a>
            <ul>
              <li class=""><a href="#icd-10--condition-list-structure">The Structure of the Condition List</a></li>
              <li class=""><a href="#icd-10--mapping-of-condition-list">Overall Mapping of the Condition List</a></li>
              <li class=""><a href="#icd-10--modifications">Modifications to Specific Conditions</a></li>
            </ul>
          </li>
          <li><a href="#icd-10-cm">Condition List and ICD-10-CM Mapping for Hosp & ED</a>
            <ul>
              <li class=""><a href="#icd-10-cm--codes">ICD-10-CM Codes</a></li>
              <li class=""><a href="#icd-10-cm--grouping">Grouping of ICD-10-CM Codes</a></li>
              <li class=""><a href="#icd-10-cm--modifications">Modifications to Specific Conditions</a></li>
            </ul>
          </li>
          <li class=""><a href="#geography-issues">Geography/GIS (State, County, Census Tract) Issues</a>
            <ul>
              <li class=""><a href="#geography-issues--state-county-designation">State and County Designation in Death Data</a></li>
              <li class=""><a href="#geography-issues--boundary-files">Boundary Files</a></li>
              <li class=""><a href="#geography-issues--census-tract-issues">Census Tract Data Issues</a></li>
            </ul>
          </li>
          <li class=""><a href="#formulas-and-measures">Formulas and Measures</a>
            <ul>
              <li class=""><a href="#formulas-and-measures--years-of-life-lost">Years of Life Lost (YLL)</a></li>
              <li class=""><a href="#formulas-and-measures--crude-rates">Crude Rates</a></li>
              <li class=""><a href="#formulas-and-measures--age-adjusted-rates">Age Adjusted Rates</a></li>
              <li class=""><a href="#formulas-and-measures--life-expectancy">Life Expectancy</a></li>
            </ul>
          </li>
        </ol>
      </nav>'
      )
    )
  )
  
}

techNotes_ui_body <- function() {
  includeMarkdown("myInfo/appText/technical1.md")
}