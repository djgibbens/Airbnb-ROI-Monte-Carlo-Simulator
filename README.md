# Airbnb-ROI-Monte-Carlo-Simulator
A Shiny-based risk assessment tool to stress-test real estate investment assumptions in Punta Cana.

# Airbnb ROI Simulator: Monte Carlo Profitability Modeling
**Technical Toolkit: R | Shiny | Stochastic Simulation | Risk Assessment**

## [Click here to launch the Live Interactive Simulator](https://djgibbens.shinyapps.io/PuntaCana_ROI_Simulator/)

## Executive Summary: "The 80% Occupancy Stress-Test"
This project was developed to perform an independent audit of real estate developer ROI claims in Punta Cana. Using a Shiny-based simulator, I modeled 5,000+ rental scenarios to validate a marketed 11.58% annual return.

**Key Finding:** The simulation confirmed that the developer’s ROI figures are mathematically accurate but rely on an 80% occupancy rate. However, local market data (TTM) indicates a median occupancy closer to 50%, highlighting a significant "optimism bias" in the investment offering.

## Technical Methodology
* **Stochastic Modeling:** Utilized a Monte Carlo approach to simulate 365-day periods, treating each night as a random booking event based on probability distributions.
* **Shiny Dashboard:** Created an interactive interface for real-time sensitivity analysis (adjusting cleaning fees, nightly rates, and occupancy).
* **Statistical Convergence:** Applied the Central Limit Theorem to generate a normal distribution of expected profits across 5,000 iterations.



## Audit & Risk Applications
* **Assumptions Validation:** Stress-testing marketing claims against historical market datasets.
* **Sensitivity Analysis:** Identifying which variables (occupancy vs. rate) have the highest impact on "Value at Risk."
* **Data Visualization:** Using histograms and density plots to communicate investment risk to non-technical stakeholders.

## Author
**David Gibbens, CAMS, CPA, MBA**
*Financial Crimes Audit Leader | Data Science Major @ ASU* - www.linkedin.com/in/djogibbens
