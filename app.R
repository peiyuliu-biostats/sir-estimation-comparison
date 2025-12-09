library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(deSolve)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(tidyr)
library(promises)
library(future)
library(future.apply)
# async strategy
plan(multisession)

# source Modules
source("R/ui_inputs.R")
source("R/ui_outputs.R")
source("R/ui_description.R")
source("R/server_inputs.R")
source("R/server_outputs.R")

 
# UI

ui <- dashboardPage(
  dashboardHeader(title = "SIR Model Estimation", titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "tabs",
                tags$li(class = "header", "SIR MODEL ANALYSIS"),
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Model Description", tabName = "model_desc", icon = icon("book-open")),
                menuItem("GitHub", icon = icon("github"), href = "https://github.com/peiyuliu-biostats/sir-estimation-comparison"),
                menuItem("shinyapps.io", icon = icon("cloud-upload-alt"), href = "https://peiyuliu.shinyapps.io/sir-estimation-comparison/"),
                menuItem("Author", tabName = "author", icon = icon("user-circle"))
    )
  ),
  dashboardBody(
    # theme custom CSS
    tags$head(tags$style(HTML("
      .skin-blue .main-header .navbar,
      .skin-blue .main-header .logo {
        background-color: #6c757d !important;
      }
      .box.box-solid.box-warning { border-color: #f0ad4e; }
      .box.box-solid.box-warning > .box-header { background: #f0ad4e; color: #ffffff; }
      .box.box-solid.box-primary { border-color: #6f42c1; }
      .box.box-solid.box-primary > .box-header { background: #6f42c1; color: #ffffff; }
      .box.box-primary:not(.box-solid) > .box-header { background-color: #f2e8f9; }
      .box.box-warning:not(.box-solid) > .box-header { background-color: #fce5d4; }
      .center-btn-wrapper { text-align: center; padding-top: 15px; padding-bottom: 15px; }
      #add_record {
        color: #fff;
        background-color: #6f42c1;
        border-color: #6f42c1;
      }
      .plot-title { text-align: left; font-size: 12px; }
      .loading-message {
        text-align: center;
        color: #6c757d;
        font-weight: bold;
        margin: 20px;
      }
      .skin-blue .main-sidebar {
        background-color: #ffffff !important;
      }
      .skin-blue .sidebar-menu > li > a {
        color: #333333 !important;
      }
      .skin-blue .sidebar-menu > li.header {
         color: #6f42c1 !important;
         background: #ffffff !important;
         font-weight: bold;
      }
      .skin-blue .sidebar-menu > li.active > a {
        background: #f2e8f9 !important;
        border-left-color: #6f42c1 !important;
        color: #333333 !important;
      }
      .skin-blue .sidebar-menu > li:hover > a {
        background: #f2e8f9 !important;
        color: #333333 !important;
      }
    "))),
    
    # assemble the body using the UI modules
    tabItems(
      # dashboard tabs
      tabItem(tabName = "dashboard",
              uimod_content_inputs(),
              uimod_content_outputs()
      ),
      
      # static content tabs
      uimod_content_doc(which = "model_desc"),
      uimod_content_doc(which = "author")
    )
  )
)


# Server 
server <- function(input, output, session) {
  
  # 1. Inputs -> Reactive (State & Core Logic)
  # this module handles data loading, computations, and state updates 
  # it returns the reactive values object containing results and flags 
  app_state <- servermod_inputs_computing(input, session)
  
  # 2. outputs (rendering)
  # takes the reactive state and renders plots/tables.
  servermod_output_render(input, output, app_state)
  
  # 3. Update UI
  # (No needed now)
  
  # 4. UX Enhancements & Performance
  # performance: `plan(multisession)` is set globally.
  # UX: progress bars and modals are handled within servermod_inputs_computing.
}

shinyApp(ui, server)