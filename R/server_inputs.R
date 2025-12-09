# R/server_inputs.R
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(tidyr)
library(deSolve)
library(future)
library(future.apply)

# ============================ 
# data Loading & Processing
# ============================ 

fetch_measles_data <- function() {
  url <- "http://kingaa.github.io/clim-dis/parest/niamey.csv"
  
  read_csv(url, show_col_types = FALSE) %>%
    filter(community == "A") %>%
    select(time = biweek, cases = measles) %>%
    arrange(time)
}

fetch_covid_data <- function() {
  url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  jhu_data <- read_csv(url, show_col_types = FALSE)
  
  # processing pipeline
  jhu_data %>%
    select(-c(UID, iso2, iso3, code3, FIPS, Admin2, Province_State,
              Country_Region, Lat, Long_, Combined_Key)) %>%
    pivot_longer(cols = matches("^\\d+/\\d+/\\d+$"), names_to = "Date", values_to = "Cumulative_Cases") %>%
    mutate(Date = mdy(Date)) %>%
    group_by(Date) %>%
    summarise(Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE), .groups = 'drop') %>%
    arrange(Date) %>%
    mutate(Daily_New_Cases = Cumulative_Cases - lag(Cumulative_Cases, default = 0)) %>%
    filter(Daily_New_Cases >= 0, Date >= as.Date("2020-01-22")) %>%
    mutate(Active_Cases = zoo::rollsum(Daily_New_Cases, k = 14, fill = NA, align = "right")) %>%
    filter(!is.na(Active_Cases)) %>%
    mutate(
      Days_Since_Start = as.numeric(Date - min(Date)),
      Biweek_Group = floor(Days_Since_Start / 14)
    ) %>%
    group_by(Biweek_Group) %>%
    summarise(
      Start_Date = min(Date),
      End_Date = max(Date),
      cases = round(mean(Active_Cases, na.rm = TRUE)/10000), # Scaling for model stability
      .groups = 'drop'
    ) %>%
    filter(Biweek_Group >= 0, cases > 0) %>%
    mutate(time = Biweek_Group + 1) %>%
    select(time, cases, Start_Date, End_Date) %>%
    arrange(time) %>%
    filter(time <= 35)
}

# ======================== 
# core Logic & modeling
# ======================== 

# SIR ODE System
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N_total <- S + I + R
    dS <- -beta * S * I / N_total
    dI <- beta * S * I / N_total - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# 1. Least Squares  
estimate_ls <- function(dataset, N, init_beta, init_gamma) {
  ls_objective <- function(params) {
    # penalize invalid parameters instead of erroring out
    if(any(params <= 0)) return(1e10) 
    
    init_state <- c(S = N - 1, I = 1, R = 0)
    # using deSolve inside
    sol <- ode(y = init_state, times = dataset$time, func = sir_model,
               parms = list(beta = params[1], gamma = params[2]))
    
    # check for simulation failures
    if(nrow(sol) != length(dataset$time)) return(1e10)
    
    predicted_I <- sol[, "I"]
    sum((dataset$cases - predicted_I)^2, na.rm = TRUE)
  }
  
  optim(par = c(init_beta, init_gamma), fn = ls_objective, 
        method = "L-BFGS-B", lower = c(0.001, 0.001))
}

# 2. Bootstrap  
estimate_bootstrap <- function(dataset, N, init_beta, init_gamma, B) {
  # parallel execution
  bootstrap_list <- future_lapply(1:B, function(i) {
    sample_indices <- sample(1:nrow(dataset), replace = TRUE)
    boot_data <- dataset[sample_indices, ]
    boot_data <- boot_data[order(boot_data$time), ]
    
    boot_obj <- function(params) {
      if(any(params <= 0)) return(1e10)
      init_state <- c(S = N - 1, I = 1, R = 0)
      sol <- ode(y = init_state, times = boot_data$time, func = sir_model,
                 parms = list(beta = params[1], gamma = params[2]))
      if(nrow(sol) != nrow(boot_data)) return(1e10)
      sum((boot_data$cases - sol[, "I"])^2, na.rm = TRUE)
    }
    
    res <- optim(par = c(init_beta, init_gamma), fn = boot_obj, 
                 method = "L-BFGS-B", lower = c(0.001, 0.001))
    
    # filter  
    if(any(res$par > 10)) return(NULL) else return(res$par)
  }, future.seed = TRUE)
  
  # clean NULLs
  valid_res <- bootstrap_list[!sapply(bootstrap_list, is.null)]
  if(length(valid_res) == 0) return(NULL)
  do.call(rbind, valid_res)
}

# 3. Metropolis-Hastings  
estimate_mcmc <- function(dataset, N, init_beta, init_gamma, M, burnin, sd_beta, sd_gamma) {
  
  mh_log_likelihood <- function(params) {
    if(any(params <= 0)) return(-Inf)
    init_state <- c(S = N - 1, I = 1, R = 0)
    
    sol <- ode(y = init_state, times = dataset$time, func = sir_model,
               parms = list(beta = params[1], gamma = params[2]))
    
    if(nrow(sol) != nrow(dataset)) return(-Inf)
    predicted_I <- sol[, "I"]
    
    # Simple Poisson likelihood
    if(any(predicted_I <= 0)) return(-Inf)
    sum(dpois(dataset$cases, lambda = predicted_I, log = TRUE), na.rm = TRUE)
  }
  
  chain <- matrix(NA, nrow = M, ncol = 3) # beta, gamma, log_post
  curr_params <- c(init_beta, init_gamma)
  curr_post <- mh_log_likelihood(curr_params)
  
  chain[1, ] <- c(curr_params, curr_post)
  accepted <- 0
  
  for(i in 2:M) {
    # Propose new parameters
    prop_params <- curr_params + rnorm(2, 0, c(sd_beta, sd_gamma))
    prop_params <- pmax(prop_params, 0.001) # Bounds
    
    prop_post <- mh_log_likelihood(prop_params)
    
    # Acceptance Step
    if(is.finite(prop_post)) {
      ratio <- exp(prop_post - curr_post)
      if(runif(1) < ratio) {
        curr_params <- prop_params
        curr_post <- prop_post
        accepted <- accepted + 1
      }
    }
    chain[i, ] <- c(curr_params, curr_post)
  }
  
  list(chain = chain, acceptance = accepted/M)
}

# wrapper to run all 3 methods for one dataset
run_full_analysis <- function(dataset, N, beta0, gamma0, B, M, burnin, sdb, sdg) {
  # 1. LS
  ls_res <- estimate_ls(dataset, N, beta0, gamma0)
  ls_out <- list(beta = ls_res$par[1], gamma = ls_res$par[2], R0 = ls_res$par[1]/ls_res$par[2])
  
  # 2. Bootstrap
  bs_mat <- estimate_bootstrap(dataset, N, beta0, gamma0, B)
  bs_out <- if(!is.null(bs_mat)) {
    list(
      beta_mean = mean(bs_mat[,1]), gamma_mean = mean(bs_mat[,2]),
      beta_ci = quantile(bs_mat[,1], c(0.025, 0.975)),
      gamma_ci = quantile(bs_mat[,2], c(0.025, 0.975)),
      bootstrap_data = bs_mat,
      R0 = mean(bs_mat[,1]) / mean(bs_mat[,2])
    )
  } else { list(beta_mean=NA, gamma_mean=NA, R0=NA) }
  
  # 3. MCMC
  mh_run <- estimate_mcmc(dataset, N, beta0, gamma0, M, burnin, sdb, sdg)
  mh_final <- mh_run$chain[(burnin + 1):M, ]
  mh_out <- list(
    beta_mean = mean(mh_final[,1]), gamma_mean = mean(mh_final[,2]),
    beta_ci = quantile(mh_final[,1], c(0.025, 0.975)),
    gamma_ci = quantile(mh_final[,2], c(0.025, 0.975)),
    chain_data = mh_final,
    full_chain = mh_run$chain,
    R0 = mean(mh_final[,1]) / mean(mh_final[,2])
  )
  
  list(ls = ls_out, bs = bs_out, mh = mh_out)
}

# ============================= 
# Module: Inputs & Computing
# =============================

servermod_inputs_computing <- function(input, session) {
  
  # 1. reactive State
  values <- reactiveValues(
    measles_data = NULL,
    covid_data = NULL,
    measles_results = list(ls = NULL, bs = NULL, mh = NULL),
    covid_results = list(ls = NULL, bs = NULL, mh = NULL),
    records = data.frame(),
    analysis_complete = FALSE,
    analysis_running = FALSE
  )
  
  # 2. initialization (Data Loading)
  observe({
    # Only load if empty
    req(is.null(values$measles_data))
    
    showModal(modalDialog("Loading initial datasets...", footer = NULL))
    
    # fetch Data
    m_data <- fetch_measles_data()
    c_data <- fetch_covid_data()
    
    if(nrow(m_data) > 0 && nrow(c_data) > 0) {
      values$measles_data <- m_data
      values$covid_data <- c_data
      removeModal()
    } else {
      removeModal()
      showNotification("Error loading data. Check internet connection.", type = "error")
    }
  })
  
  # 3. event: Run Analysis
  observeEvent(input$add_record, {
    req(values$measles_data, values$covid_data)
    
    values$analysis_running <- TRUE
    values$analysis_complete <- FALSE
    
    withProgress(message = 'Running Analysis...', value = 0, {
      
      # Measles
      incProgress(0.1, detail = "Fitting Measles Model...")
      m_res <- run_full_analysis(
        values$measles_data, input$N_0_m, input$beta_0_m, input$gamma_0_m,
        input$B_m, input$M_m, input$M_burnin_m, input$sd_beta_m, input$sd_gamma_m
      )
      values$measles_results <- m_res
      
      # COVID-19
      incProgress(0.5, detail = "Fitting COVID-19 Model...")
      c_res <- run_full_analysis(
        values$covid_data, input$N_0_c, input$beta_0_c, input$gamma_0_c,
        input$B_c, input$M_c, input$M_burnin_c, input$sd_beta_c, input$sd_gamma_c
      )
      values$covid_results <- c_res
      
      incProgress(0.9, detail = "Saving Records...")
      
      # create Record Rows
      rec_m <- data.frame(
        Dataset = "Measles",
        LS_Beta = round(m_res$ls$beta, 4), LS_Gamma = round(m_res$ls$gamma, 4), LS_R0 = round(m_res$ls$R0, 2),
        BS_Beta = round(m_res$bs$beta_mean, 4), BS_Gamma = round(m_res$bs$gamma_mean, 4), BS_R0 = round(m_res$bs$R0, 2),
        MH_Beta = round(m_res$mh$beta_mean, 4), MH_Gamma = round(m_res$mh$gamma_mean, 4), MH_R0 = round(m_res$mh$R0, 2),
        stringsAsFactors = FALSE
      )
      
      rec_c <- data.frame(
        Dataset = "COVID-19",
        LS_Beta = round(c_res$ls$beta, 4), LS_Gamma = round(c_res$ls$gamma, 4), LS_R0 = round(c_res$ls$R0, 2),
        BS_Beta = round(c_res$bs$beta_mean, 4), BS_Gamma = round(c_res$bs$gamma_mean, 4), BS_R0 = round(c_res$bs$R0, 2),
        MH_Beta = round(c_res$mh$beta_mean, 4), MH_Gamma = round(c_res$mh$gamma_mean, 4), MH_R0 = round(c_res$mh$R0, 2),
        stringsAsFactors = FALSE
      )
      
      values$records <- rbind(values$records, rec_m, rec_c)
      values$analysis_complete <- TRUE
      values$analysis_running <- FALSE
    })
    
    showNotification("Analysis completed successfully!", type = "message")
  })
  
  return(values)
}