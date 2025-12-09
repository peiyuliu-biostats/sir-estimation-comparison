# R/ui_outputs.R
# uimodule3: Contains all output placeholders for results.

uimod_content_outputs <- function() {
  tagList(
    # -- records table Row --
    fluidRow(
      box(title = "Analysis Records", status = "primary", solidHeader = FALSE, 
          width = 12, collapsible = TRUE, 
          DT::dataTableOutput("parameter_records"))
    ),
    
    # -- visual Results row --
    fluidRow(
      column(width = 6, 
             box(title = "Measles Results", status = "primary", solidHeader = FALSE, 
                 width = NULL,
                 uiOutput("measles_tabs_ui"))),
      column(width = 6, 
             box(title = "COVID-19 Results", status = "warning", solidHeader = FALSE, 
                 width = NULL,
                 uiOutput("covid_tabs_ui")))
    )
  )
}