# R/server_outputs.R
library(shiny)
library(plotly)
library(DT)
library(deSolve)  

# ======================================== 
# functinos for Visualization & Plotting
# ======================================== 

# deterministic SIR (Needed here for generating fit lines)
sir_model_vis <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_total <- S + I + R
    dS <- -beta * S * I / N_total
    dI <- beta * S * I / N_total - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# 1. comparison Plot (Data vs LS vs Bootstrap vs MH)
create_fit_plot <- function(data_set, results, N, name, cols) {
  if(is.null(results$ls)) return(plotly_empty() %>% layout(title="No Data"))
  
  time_seq <- seq(min(data_set$time), max(data_set$time), length.out = 100)
  init <- c(S = N - 1, I = 1, R = 0)
  
  # solve ODEs for means
  sol_ls <- ode(y=init, times=time_seq, func=sir_model_vis, parms=list(beta=results$ls$beta, gamma=results$ls$gamma))
  sol_bs <- ode(y=init, times=time_seq, func=sir_model_vis, parms=list(beta=results$bs$beta_mean, gamma=results$bs$gamma_mean))
  sol_mh <- ode(y=init, times=time_seq, func=sir_model_vis, parms=list(beta=results$mh$beta_mean, gamma=results$mh$gamma_mean))
  
  # calculate CI for Bootstrap
  bs_samps <- results$bs$bootstrap_data
  bs_preds <- apply(bs_samps, 1, function(p) {
    ode(y=init, times=time_seq, func=sir_model_vis, parms=list(beta=p[1], gamma=p[2]))[,"I"]
  })
  bs_low <- apply(bs_preds, 1, quantile, 0.025)
  bs_upp <- apply(bs_preds, 1, quantile, 0.975)
  
  # CI for MH (Subsample for speed)
  mh_samps <- results$mh$chain_data[,1:2]
  sub_idx <- sample(nrow(mh_samps), min(200, nrow(mh_samps)))
  mh_preds <- apply(mh_samps[sub_idx,], 1, function(p) {
    ode(y=init, times=time_seq, func=sir_model_vis, parms=list(beta=p[1], gamma=p[2]))[,"I"]
  })
  mh_low <- apply(mh_preds, 1, quantile, 0.025)
  mh_upp <- apply(mh_preds, 1, quantile, 0.975)
  
  plot_ly() %>%
    add_markers(data=data_set, x=~time, y=~cases, name="Observed", marker=list(color="black", size=8), legendgroup="g1") %>%
    # LS
    add_lines(x=time_seq, y=sol_ls[,"I"], name="Least Squares", line=list(color=cols[1], width=3), legendgroup="g1") %>%
    # MH
    add_ribbons(x=time_seq, ymin=mh_low, ymax=mh_upp, name="MH 95% CI", fillcolor="rgba(50,205,50,0.2)", line=list(color="transparent"), legendgroup="g2") %>%
    add_lines(x=time_seq, y=sol_mh[,"I"], name="MCMC Mean", line=list(color=cols[3], width=3, dash="dot"), legendgroup="g2") %>%
    # Bootstrap
    add_ribbons(x=time_seq, ymin=bs_low, ymax=bs_upp, name="BS 95% CI", fillcolor="rgba(70,130,180,0.2)", line=list(color="transparent"), legendgroup="g3") %>%
    add_lines(x=time_seq, y=sol_bs[,"I"], name="Bootstrap Mean", line=list(color=cols[2], width=3, dash="dash"), legendgroup="g3") %>%
    layout(
      title = paste(name, "Fit Comparison"),
      xaxis = list(title = "Time"), yaxis = list(title = "Cases"),
      legend = list(orientation = "h", x=0.5, y=-0.2, xanchor="center")
    )
}

# 2. MCMC trace plot
create_trace_plot <- function(results, name) {
  if(is.null(results$mh)) return(plotly_empty())
  chain <- results$mh$chain_data
  
  p1 <- plot_ly(y=chain[,1], type="scatter", mode="lines", name="Beta", line=list(color="darkred"))
  p2 <- plot_ly(y=chain[,2], type="scatter", mode="lines", name="Gamma", line=list(color="darkgreen"))
  
  subplot(p1, p2, nrows=2, shareX=TRUE) %>%
    layout(title=paste(name, "MCMC Trace"), showlegend=FALSE)
}

# 3. Posterior density  
create_posterior_plot <- function(results, name) {
  if(is.null(results$mh)) return(plotly_empty())
  chain <- results$mh$chain_data
  
  p1 <- plot_ly(x=chain[,1], type="histogram", name="Beta", marker=list(color="pink", opacity=0.7)) %>%
    add_segments(x=results$mh$beta_mean, xend=results$mh$beta_mean, y=0, yend=100, line=list(color="red", dash="dash")) %>%
    layout(xaxis=list(title="Beta"))
  
  p2 <- plot_ly(x=chain[,2], type="histogram", name="Gamma", marker=list(color="lightcoral", opacity=0.7)) %>%
    add_segments(x=results$mh$gamma_mean, xend=results$mh$gamma_mean, y=0, yend=100, line=list(color="red", dash="dash")) %>%
    layout(xaxis=list(title="Gamma"))
  
  subplot(p1, p2, nrows=2, shareY=FALSE) %>%
    layout(title=paste(name, "Posterior Density"), showlegend=FALSE)
}

# 4. Bootstrap distribution 
create_bs_dist_plot <- function(results, name) {
  if(is.null(results$bs)) return(plotly_empty())
  bs_data <- results$bs$bootstrap_data
  
  d_beta <- density(bs_data[,1])
  d_gamma <- density(bs_data[,2])
  
  p1 <- plot_ly(x=d_beta$x, y=d_beta$y, type="scatter", mode="lines", fill="tozeroy", name="Beta", line=list(color="steelblue")) %>%
    layout(xaxis=list(title="Beta"))
  p2 <- plot_ly(x=d_gamma$x, y=d_gamma$y, type="scatter", mode="lines", fill="tozeroy", name="Gamma", line=list(color="limegreen")) %>%
    layout(xaxis=list(title="Gamma"))
  
  subplot(p1, p2, nrows=2, shareY=FALSE) %>%
    layout(title=paste(name, "Bootstrap Distribution"), showlegend=FALSE)
}

# ============================= 
# Module: Outputs & rendering
# ============================= 

servermod_output_render <- function(input, output, reactives) {
  
  # 1. render Record Table
  output$parameter_records <- DT::renderDataTable({
    req(nrow(reactives$records) > 0)
    DT::datatable(reactives$records, selection = "single", 
                  options = list(scrollX = TRUE, pageLength = 5))
  })
  
  # 2. render Measles Tabs UI
  output$measles_tabs_ui <- renderUI({
    if(reactives$analysis_running && !reactives$analysis_complete) {
      div(class = "loading-message", h4("Analysis in Progress..."), p("Calculating Measles Model..."))
    } else if(reactives$analysis_complete) {
      tabsetPanel(id = "measles_tabs",
                  tabPanel("Fit Comparison", plotlyOutput("measles_fit_plot", height = "350px")),
                  tabPanel("MCMC Trace", plotlyOutput("measles_mcmc_trace", height = "350px")),
                  tabPanel("MH Posterior", plotlyOutput("measles_mh_posterior", height = "350px")),
                  tabPanel("Bootstrap Distribution", plotlyOutput("measles_bs_dist", height = "350px"))
      )
    } else {
      div(class = "loading-message", p("Click 'Add Records & Run Analysis' to begin."))
    }
  })
  
  # 3. render COVID Tabs UI
  output$covid_tabs_ui <- renderUI({
    if(reactives$analysis_running && !reactives$analysis_complete) {
      div(class = "loading-message", h4("Analysis in Progress..."), p("Calculating COVID-19 Model..."))
    } else if(reactives$analysis_complete) {
      tabsetPanel(id = "covid_tabs",
                  tabPanel("Fit Comparison", plotlyOutput("covid_fit_plot", height = "350px")),
                  tabPanel("MCMC Trace", plotlyOutput("covid_mcmc_trace", height = "350px")),
                  tabPanel("MH Posterior", plotlyOutput("covid_mh_posterior", height = "350px")),
                  tabPanel("Bootstrap Distribution", plotlyOutput("covid_bs_dist", height = "350px"))
      )
    } else {
      div(class = "loading-message", p("Click 'Add Records & Run Analysis' to begin."))
    }
  })
  
  # 4. render Plots (Measles)
  output$measles_fit_plot <- renderPlotly({
    req(reactives$analysis_complete)
    create_fit_plot(reactives$measles_data, reactives$measles_results, input$N_0_m, "Measles", c("red", "blue", "green"))
  }) %>% bindCache(reactives$measles_results, input$N_0_m)
  
  output$measles_mcmc_trace <- renderPlotly({
    req(reactives$analysis_complete)
    create_trace_plot(reactives$measles_results, "Measles")
  }) %>% bindCache(reactives$measles_results)
  
  output$measles_mh_posterior <- renderPlotly({
    req(reactives$analysis_complete)
    create_posterior_plot(reactives$measles_results, "Measles")
  }) %>% bindCache(reactives$measles_results)
  
  output$measles_bs_dist <- renderPlotly({
    req(reactives$analysis_complete)
    create_bs_dist_plot(reactives$measles_results, "Measles")
  }) %>% bindCache(reactives$measles_results)
  
  # 5. Render Plots (COVID)
  output$covid_fit_plot <- renderPlotly({
    req(reactives$analysis_complete)
    create_fit_plot(reactives$covid_data, reactives$covid_results, input$N_0_c, "COVID-19", c("darkred", "darkblue", "darkgreen"))
  }) %>% bindCache(reactives$covid_results, input$N_0_c)
  
  output$covid_mcmc_trace <- renderPlotly({
    req(reactives$analysis_complete)
    create_trace_plot(reactives$covid_results, "COVID-19")
  }) %>% bindCache(reactives$covid_results)
  
  output$covid_mh_posterior <- renderPlotly({
    req(reactives$analysis_complete)
    create_posterior_plot(reactives$covid_results, "COVID-19")
  }) %>% bindCache(reactives$covid_results)
  
  output$covid_bs_dist <- renderPlotly({
    req(reactives$analysis_complete)
    create_bs_dist_plot(reactives$covid_results, "COVID-19")
  }) %>% bindCache(reactives$covid_results)
}