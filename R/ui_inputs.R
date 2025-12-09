# /R/ui_inputs.R
# ui2: all input controls, the action button, and the records table.

# function for consistent parameter inputs with tooltips
param_input_factory <- function(inputId, label_symbol, tooltip_text, value, min, max, step) {
  div(style = "display: flex; align-items: center; margin-bottom: 5px;",
      div(style = "width: 50%;",
          tags$label(HTML(label_symbol), style = "margin-right: 5px;"),
          tags$i(class = "fa fa-info-circle",
                 style = "color: #007bff; cursor: help;",
                 title = tooltip_text,
                 `data-toggle` = "tooltip",
                 `data-placement` = "top")),
      div(style = "width: 50%;",
          numericInput(inputId, label = NULL, value = value, min = min, max = max, step = step)))
}

uimod_content_inputs <- function() {
  tagList(
    # -- Parameter Settings Row --
    fluidRow(
      box(title = "Measles Parameters Setting", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 6,
          param_input_factory("N_0_m", "N_0:", "Total population size", 3500, 3000, 4000, 100),
          param_input_factory("beta_0_m", "&beta;<sub>0</sub>:", "Initial transmission rate", 0.2, 0.05, 0.55, 0.01),
          param_input_factory("gamma_0_m", "&gamma;<sub>0</sub>:", "Initial recovery rate", 0.21, 0.05, 0.55, 0.01),
          param_input_factory("B_m", "B:", "Bootstrap sample size", 300, 50, 1000, 10),
          param_input_factory("M_m", "M:", "Iteration times of MH", 3000, 500, 100000, 1000),
          param_input_factory("M_burnin_m", "M_burnin:", "Burn-in period for MCMC", 2000, 100, 10000, 500),
          param_input_factory("sd_beta_m", "SD_&beta;:", "Proposal SD for &beta; in MH", 0.014, 0.001, 0.1, 0.001),
          param_input_factory("sd_gamma_m", "SD_&gamma;:", "Proposal SD for &gamma; in MH", 0.014, 0.001, 0.1, 0.001)),
      
      box(title = "COVID-19 Parameters Setting", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = 6,
          param_input_factory("N_0_c", "N_0:", "Total population size (in 10k)", 32800, 32000, 36000, 100),
          param_input_factory("beta_0_c", "&beta;<sub>0</sub>:", "Initial transmission rate", 0.2, 0.05, 0.55, 0.01),
          param_input_factory("gamma_0_c", "&gamma;<sub>0</sub>:", "Initial recovery rate", 0.01, 0.01, 0.55, 0.01),
          param_input_factory("B_c", "B:", "Bootstrap sample size", 300, 50, 1000, 10),
          param_input_factory("M_c", "M:", "Iteration times of MH", 3000, 500, 100000, 1000),
          param_input_factory("M_burnin_c", "M_burnin:", "Burn-in period for MCMC", 2000, 100, 10000, 500),
          param_input_factory("sd_beta_c", "SD_&beta;:", "Proposal SD for &beta; in MH", 0.01, 0.001, 0.1, 0.001),
          param_input_factory("sd_gamma_c", "SD_&gamma;:", "Proposal SD for &gamma; in MH", 0.01, 0.001, 0.1, 0.001))
    ),
    
    # -- Action Button Row --
    div(class = "center-btn-wrapper",
        actionButton("add_record", "Add Records & Run Analysis", icon = icon("plus"))
    )
  )
}