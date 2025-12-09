# R/ui_description.R
# uimodule1: specific static content page (tabItem) based on the 'which'  

uimod_content_doc <- function(which = c("model_desc", "author")) {
  which <- match.arg(which)
  
  if (which == "model_desc") {
    # --- description Tab ---
    tabItem(tabName = "model_desc",
            fluidRow(
              box(
                title = "Model & Methods Description", status = "primary", solidHeader = TRUE, width = 12,
                withMathJax(),
                h2("Overview"),
                p("This document describes the underlying models and statistical methods used in the 'SIR Model Estimation' Shiny application. The primary goal of this project is to compare the performance of different parameter estimation techniques when applied to two distinct types of epidemiological models: a", strong("Deterministic SIR Model"), "and a", strong("Stochastic SIR Model"),"."),
                p("The analysis is performed on two real-world datasets:"),
                tags$ol(
                  tags$li(strong("Measles Data:"), "A classic dataset of measles cases in Niamey, Niger."),
                  tags$li(strong("COVID-19 Data:"), "Aggregated data of active COVID-19 cases in the United States during the early phase of the pandemic.")
                ),
                p("This report will detail the mathematical formulation of each model, the fitting procedures employed, and present the final results for both datasets."),
                hr(),
                h2("1. The Deterministic SIR Model (ODE)"),
                p("The deterministic Susceptible-Infected-Recovered (SIR) model is a cornerstone of mathematical epidemiology. It describes the flow of individuals between three compartments using a set of Ordinary Differential Equations (ODEs). This model assumes a large, well-mixed population where randomness can be averaged out."),
                h3("Model Equations"),
                p("The dynamics of the system are governed by the following equations:"),
                withMathJax(p("$$ \\frac{dS}{dt} = -\\beta \\frac{S \\cdot I}{N} $$")),
                withMathJax(p("$$ \\frac{dI}{dt} = \\beta \\frac{S \\cdot I}{N} - \\gamma I $$")),
                withMathJax(p("$$ \\frac{dR}{dt} = \\gamma I $$")),
                p("Where:"),
                tags$ul(
                  tags$li(strong("S(t):"), "Number of susceptible individuals at time t."),
                  tags$li(strong("I(t):"), "Number of infected individuals at time t."),
                  tags$li(strong("R(t):"), "Number of recovered (and immune) individuals at time t."),
                  tags$li(strong("N:"), "Total population size (N = S + I + R, assumed constant)."),
                  tags$li(strong(withMathJax("\\(\\beta\\) (beta):")), "The transmission rate."),
                  tags$li(strong(withMathJax("\\(\\gamma\\) (gamma):")), "The recovery rate (inverse of the infectious period).")
                ),
                withMathJax(p("The basic reproduction number, \\(R_0 = \\frac{\\beta}{\\gamma}\\), represents the average number of secondary infections produced by a single infected individual in a completely susceptible population.")),
                h3("Parameter Estimation Methods"),
                withMathJax(p("The Shiny application implements three distinct methods to estimate the parameters \\(\\beta\\) and \\(\\gamma\\) from the observed case data.")),
                tags$ol(
                  tags$li(strong("Least Squares (LS):"), withMathJax("This is the simplest method. It uses an optimization algorithm `L-BFGS-B`) to find the parameter values (\\(\\beta, \\gamma\\)) that minimize the sum of squared differences between the model's predicted number of infected individuals, \\(I(t)\\), and the observed case counts.")),
                  tags$li(strong("Bootstrap:"), "This method assesses the uncertainty in the parameter estimates. It involves repeatedly resampling the original dataset (with replacement) to create many new, slightly different datasets. The LS method is then applied to each of these new datasets. The distribution of the resulting parameter estimates provides a measure of their stability and is used to calculate confidence intervals."),
                  tags$li(strong("Metropolis-Hastings (MCMC):"), "This is a Bayesian approach. It aims to find the entire posterior probability distribution of the parameters, given the data. It starts with an initial guess and 'walks' randomly through the parameter space, preferentially moving towards regions of higher likelihood. The collection of points from this 'walk' (the Markov chain) forms the posterior distribution, from which we can calculate means and credible intervals.")
                ),
                hr(),
                h2("2. The Stochastic SIR Model"),
                p("While the deterministic model describes the average behavior of a large population, the", withMathJax("Stochastic SIR Model"), "captures the inherent randomness and chance in disease transmission, especially in smaller populations. Instead of smooth flows, it models individual infection and recovery events."),
                h3("The Gillespie Algorithm"),
                p("Our stochastic model is implemented using the Gillespie Direct Method, an event-based algorithm. The core idea is:"),
                tags$ol(
                  tags$li(strong("Define Events and Rates:"), withMathJax("At any given moment, there are two possible events that can happen: an"), withMathJax("Infection"), withMathJax("(rate \\(\\lambda_{inf} = \\beta \\frac{S \\cdot I}{N}\\)) and a"), withMathJax("Recovery"), withMathJax("(rate \\(\\lambda_{rec} = \\gamma I\\)).")),
                  tags$li(strong("Simulate Time to Next Event:"), withMathJax("The time until the next event, \\(\\Delta t\\), is a random variable drawn from an exponential distribution with a rate equal to the total event rate, \\(\\lambda_{total} = \\lambda_{inf} + \\lambda_{rec}\\)."), withMathJax("$$\\Delta t \\sim \\text{Exponential}(\\lambda_{total})$$")),
                  tags$li(strong("Simulate Which Event Occurs:"), withMathJax("The probability of the next event being an infection is proportional to its rate: \\(P(\\text{Infection}) = \\frac{\\lambda_{inf}}{\\lambda_{total}}\\).")),
                  tags$li(strong("Update State and Repeat:"), withMathJax("The system time is advanced by \\(\\Delta t\\), the counts of S, I, and R are updated, and the process repeats until the epidemic ends (I=0)."))
                ),
                p("Because this model is random, running it once gives only one possible trajectory. To get a stable prediction, we must run it many times (e.g., 100 simulations) and average the number of infected individuals at each time point."),
                h3("Parameter Estimation & Fitting Results"),
                p("For the stochastic model, we use the average of multiple simulations as the prediction and fit the parameters using Least Squares and Bootstrap. You can review the full analysis with fitting results in the following HTML files:"),
                tags$ul(
                  tags$li(tags$a("Full Stochastic Model Analysis for Measles", href="measles_stochastic.html", target="_blank", rel="noopener noreferrer")),
                  tags$li(tags$a("Full Stochastic Model Analysis for COVID-19", href="covid_stochastic.html", target="_blank", rel="noopener noreferrer"))
                ),
                hr(),
                h2("3. Model Comparison and Conclusion"),
                tags$ul(
                  tags$li(strong("Deterministic Model:"), "It's computationally fast and provides a good overview of the epidemic's average trend. It is well-suited for large populations where random fluctuations are minimal. The variety of fitting methods available (LS, Bootstrap, MCMC) allows for a deep analysis of parameter uncertainty."),
                  tags$li(strong("Stochastic Model:"), "It provides a more realistic simulation of disease spread by modeling individual chance events. This is particularly important for smaller populations or when studying the probability of epidemic fade-out. However, it is computationally intensive, as it requires averaging many simulations for a stable prediction, making methods like MCMC impractical.")
                )
              )
            )
    )
  } else if (which == "author") {
    # --- author tab ---
    tabItem(tabName = "author",
            fluidRow(
              box(
                title = "About The Author",
                solidHeader = TRUE,
                status = "primary",
                width = 12,
                h3("Author Information"),
                p("This application for SIR model estimation and comparison was developed by:"),
                tags$ul(
                  tags$li(HTML("<b>Author:</b> Peiyu Liu")),
                  tags$li(HTML("<b>Affiliation:</b> Department of Biostatistics, University of Florida")),
                  tags$li(HTML("<b>Contact:</b> <a href='mailto:pyliu0620@outlook.com'>pyliu0620@outlook.com</a>")),
                  tags$li(HTML("<b>GitHub:</b> <a href='https://github.com/peiyuliu-biostats' target='_blank'>https://github.com/peiyuliu-biostats</a>"))
                )
              )
            )
    )
  }
}