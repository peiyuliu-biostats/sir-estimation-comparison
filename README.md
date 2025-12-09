# SIR Model Estimation Comparison App

A Shiny application to fit, compare, and visualize parameter estimation methods for the SIR epidemiological model using real-world data.

➡️ [**View the Live Application Here**](https://peiyuliu.shinyapps.io/sir-estimation-comparison/)

*(Note: The live application runs on a free server. The analysis is computationally intensive, so please be patient as it may take a few minutes (about 4 mins for windows user and 1min for mac user)to complete after clicking "Run Analysis".)*

---

## Description

This application is designed to explore and understand the nuances of fitting mathematical and stochastic models to infectious disease data. Users can adjust a range of parameters for the SIR model and the fitting algorithms to see how they impact the results when applied to two distinct real-world datasets: a classic Measles outbreak and the early phase of the COVID-19 pandemic in the US.

The tool provides a dynamic interface to visually compare the outcomes of three estimation techniques of Least square, Bootstrap and MCMC, offering insights into their performance, stability, and the uncertainty.

## Features

*   **Simultaneously Analyze Two Datasets**: Compare model fitting results for both Measles and COVID-19.

*   **Interactively Control Parameters**: Adjust key parameters for the SIR model and the fitting methods, including population size, initial parameter guesses, and algorithm-specific settings like Bootstrap iterations (`B`) and MCMC chain length (`M`).

*   **Compare Three Core Estimation Methods**:
    1.  **Least Squares (LS)**: A fast, deterministic optimization method that provides a good point estimate.
    2.  **Bootstrap**: A resampling method used to quantify the uncertainty of the parameter estimates by analyzing the model's performance on many slightly different versions of the data.
    3.  **Metropolis-Hastings (MCMC)**: A powerful Bayesian method that explores the entire parameter space to generate a full posterior distribution for the model parameters.

*   **Comprehensive Visualization Suite**: For each dataset, the application generates a suite of four diagnostic plots:
    -   **Fit Comparison**: Overlays the observed data with the fitted curves from all three methods, including 95% confidence/credible intervals.
    -   **MCMC Trace Plot**: Visualizes the path of the Markov chain to help diagnose convergence.
    -   **MH Posterior Distribution**: A histogram showing the final distribution of the MCMC samples after the burn-in period.
    -   **Bootstrap Distribution**: A smooth density plot showing the distribution of the parameter estimates from the bootstrap procedure.

## Technical Details

The application is built in R using the Shiny framework. To handle the computationally intensive fitting procedures without freezing the user interface or timing out the server, the backend leverages the `{promises}` and `{future}` packages for asynchronous programming. When an analysis is run, the calculations for each dataset are sent to parallel background R sessions, allowing the main application to remain responsive.

### Data Sources
*   **Measles Data**: `kingaa` R package, originally from a 1995 epidemic in Niamey, Niger.
*   **COVID-19 Data**: public GitHub repository maintained by the Johns Hopkins University Center for Systems Science and Engineering (CSSE).

---
