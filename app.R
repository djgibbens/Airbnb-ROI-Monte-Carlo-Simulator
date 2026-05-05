# ---- LIBRARIES ----
library(shiny)
library(ggplot2)
library(dplyr)
library(shinyBS)
library(scales)

# ---- LOAD DATA ----
pdc <- read.csv("PuntCanaListingsData-AirRoi.com-04-04-2026.csv")

pdc <- pdc %>%
  filter(!is.na(listing_name) & listing_name != "") %>%
  mutate(
    listing_label = paste0("TTM $", round(ttm_avg_rate), " – ", listing_name)
  )

# ---- UI ----
ui <- fluidPage(
  titlePanel(
    HTML("Punta Cana Airbnb ROI Simulator<br><span style='font-size:16px;'>by David J. Gibbens</span>")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Listing selector
      selectInput(
        "listing",
        "Choose a listing:",
        choices  = setNames(pdc$listing_name, pdc$listing_label),
        selected = pdc$listing_name[1]
      ),
      
      # Listing card (boxed)
      uiOutput("listing_card"),
      tags$hr(),
      
      # TTM vs L90D toggle
      radioButtons(
        "period",
        "Performance window:",
        choices  = c("TTM (last 12 months)" = "TTM",
                     "L90D (last 90 days)"  = "L90D"),
        selected = "TTM"
      ),
      
      # Seasonality slider
      sliderInput(
        "seasonality",
        "Seasonality multiplier:",
        min = 0.5, max = 1.5, value = 1, step = 0.05
      ),
      
      # Nightly rate slider
      sliderInput(
        "nightly_rate",
        "Base nightly rate (USD):",
        min = 20, max = 2000, value = 200, step = 10
      ),
      
      # Occupancy slider
      sliderInput(
        "occupancy",
        "Occupancy rate (0–1):",
        min = 0, max = 1, value = 0.5, step = 0.05
      ),
      
      # Cleaning fee slider
      sliderInput(
        "cleaning_fee",
        "Cleaning fee per stay (USD):",
        min = 0, max = 300, value = 50, step = 5
      ),
      
      # Number of simulations
      sliderInput(
        "n_sims",
        "Number of simulations:",
        min = 200, max = 5000, value = 1000, step = 100
      ),
      
      # Number of days
      sliderInput(
        "n_days",
        "Number of days in period:",
        min = 7, max = 365, value = 30, step = 1
      )
    ),
    
    mainPanel(
      h3("Simulated Profit Distribution"),
      plotOutput("profit_plot"),
      br(),
      h4("Summary Statistics"),
      verbatimTextOutput("summary_text"),
      
      br(), br(),
      h4("Slider Guide & Assumptions"),
      tags$ul(
        tags$li(tags$strong("Performance Window:"),
                "Toggle between TTM (Trailing Twelve Months) and L90D (Last 90 Days). This sets which historical period the pre-populated values are pulled from."),
        tags$li(tags$strong("Seasonality Multiplier:"),
                "Adjusts the nightly rate up or down to reflect high season (>1.0) or low season (<1.0). A value of 1.0 means no seasonal adjustment."),
        tags$li(tags$strong("Base Nightly Rate (USD):"),
                "The average nightly price for this listing. Auto-populated from the CSV dataset. Typical Punta Cana listings range from $150–$250/night."),
        tags$li(tags$strong("Occupancy Rate (0–1):"),
                "The probability a given night is booked. Auto-populated from the dataset. A realistic range is 0.55–0.70 (55–70%)."),
        tags$li(tags$strong("Cleaning Fee per Stay (USD):"),
                "The cleaning cost deducted per stay. Auto-populated from the dataset. Most listings charge $40–$80 per cleaning. The simulator assumes 1 cleaning per 4-night stay."),
        tags$li(tags$strong("Number of Simulations:"),
                "How many random scenarios to generate. Higher values (1,000–5,000) produce smoother, more reliable distributions."),
        tags$li(tags$strong("Number of Days in Period:"),
                "The time horizon for the simulation. Use 30 for a monthly snapshot. Increase to 90 or 365 for a smoother bell-shaped curve — this is the Central Limit Theorem in action.")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Listing card
  output$listing_card <- renderUI({
    req(input$listing)
    row <- pdc[pdc$listing_name == input$listing, ][1, ]
    
    wellPanel(
      tags$strong("Listing Summary"),
      tags$hr(),
      tags$p(paste0("Listing: ", row$listing_name)),
      tags$p(paste0("Bedrooms: ", row$bedrooms)),
      tags$p(paste0("Baths: ", row$baths)),
      tags$p(paste0("Room Type: ", row$room_type)),
      tags$p(paste0("Overall Rating: ", round(row$rating_overall, 2), " ⭐"))
    )
  })
  
  # Auto-fill sliders when listing or period changes
  observeEvent(list(input$listing, input$period), {
    req(input$listing)
    row <- pdc[pdc$listing_name == input$listing, ][1, ]
    
    if (input$period == "TTM") {
      base_rate <- row$ttm_avg_rate
      occ_val   <- row$ttm_occupancy
    } else {
      base_rate <- row$l90d_avg_rate
      occ_val   <- row$l90d_occupancy
    }
    
    if (!is.na(base_rate)) {
      updateSliderInput(session, "nightly_rate", value = base_rate)
    }
    if (!is.na(occ_val)) {
      updateSliderInput(session, "occupancy", value = occ_val)
    }
    if (!is.na(row$cleaning_fee)) {
      updateSliderInput(session, "cleaning_fee", value = row$cleaning_fee)
    }
  })
  
  # Simulation
  sim_results <- reactive({
    n_sims <- input$n_sims
    n_days <- input$n_days
    occ    <- input$occupancy
    rate   <- input$nightly_rate
    clean  <- input$cleaning_fee
    seas   <- input$seasonality
    
    eff_rate <- rate * seas
    
    booked_nights <- rbinom(n_sims, size = n_days, prob = occ)
    
    avg_stay <- 4
    estimated_stays <- booked_nights / avg_stay
    cleaning_cost   <- estimated_stays * clean
    
    revenue <- booked_nights * eff_rate
    profit  <- revenue - cleaning_cost
    
    profit_per_night <- ifelse(booked_nights > 0, profit / booked_nights, NA)
    profit_per_stay  <- ifelse(estimated_stays > 0, profit / estimated_stays, NA)
    
    data.frame(
      sim              = 1:n_sims,
      booked_nights    = booked_nights,
      estimated_stays  = estimated_stays,
      revenue          = revenue,
      cleaning_cost    = cleaning_cost,
      profit           = profit,
      profit_per_night = profit_per_night,
      profit_per_stay  = profit_per_stay
    )
  })
  
  # ---- UPGRADED PLOT ----
  output$profit_plot <- renderPlot({
    df <- sim_results()
    
    mean_val   <- mean(df$profit, na.rm = TRUE)
    median_val <- median(df$profit, na.rm = TRUE)
    p5_val     <- quantile(df$profit, 0.05, na.rm = TRUE)
    p95_val    <- quantile(df$profit, 0.95, na.rm = TRUE)
    
    ggplot(df, aes(x = profit)) +
      
      # Shaded 90% confidence band
      annotate("rect",
               xmin = p5_val, xmax = p95_val,
               ymin = -Inf, ymax = Inf,
               fill = "#0072B2", alpha = 0.08) +
      
      # Histogram bars
      geom_histogram(aes(y = after_stat(count)),
                     bins = 35, fill = "#0072B2",
                     color = "#004a75", alpha = 0.75, linewidth = 0.3) +
      
      # Mean line (red dashed)
      geom_vline(xintercept = mean_val,
                 color = "#D62828", linetype = "dashed", linewidth = 1.1) +
      
      # Median line (green dotted)
      geom_vline(xintercept = median_val,
                 color = "#2E8B57", linetype = "dotted", linewidth = 1.1) +
      
      # Mean label
      annotate("label", x = mean_val, y = Inf, vjust = 1.5,
               label = paste0("Mean: $", formatC(mean_val, format = "f", digits = 0, big.mark = ",")),
               color = "#D62828", fill = "white", size = 4,
               fontface = "bold", label.size = 0.4, label.padding = unit(0.3, "lines")) +
      
      # Median label
      annotate("label", x = median_val, y = Inf, vjust = 3.2,
               label = paste0("Median: $", formatC(median_val, format = "f", digits = 0, big.mark = ",")),
               color = "#2E8B57", fill = "white", size = 4,
               fontface = "bold", label.size = 0.4, label.padding = unit(0.3, "lines")) +
      
      # P5 / P95 labels at bottom
      annotate("text", x = p5_val, y = 0, vjust = -0.5,
               label = paste0("P5: $", formatC(p5_val, format = "f", digits = 0, big.mark = ",")),
               color = "#555555", size = 3.2, fontface = "italic") +
      annotate("text", x = p95_val, y = 0, vjust = -0.5,
               label = paste0("P95: $", formatC(p95_val, format = "f", digits = 0, big.mark = ",")),
               color = "#555555", size = 3.2, fontface = "italic") +
      
      # Dollar-formatted x-axis with more breaks
      scale_x_continuous(
        labels = scales::dollar_format(),
        n.breaks = 10
      ) +
      
      labs(
        x        = "Profit over Selected Period (USD)",
        y        = "Simulation Count",
        title    = "Simulated Profit Distribution",
        subtitle = "Shaded region = 90% of simulated outcomes (P5 to P95)"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        plot.title         = element_text(face = "bold", size = 18, color = "#1a1a1a"),
        plot.subtitle      = element_text(size = 12, color = "#666666", margin = margin(b = 12)),
        axis.title         = element_text(face = "bold", size = 13),
        axis.text          = element_text(size = 11, color = "#333333"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin        = margin(15, 20, 10, 15)
      )
  })
  
  # ---- SUMMARY STATS (clean labels) ----
  output$summary_text <- renderPrint({
    df <- sim_results()
    
    stats_profit <- c(
      mean   = mean(df$profit, na.rm = TRUE),
      median = median(df$profit, na.rm = TRUE),
      sd     = sd(df$profit, na.rm = TRUE),
      p5     = unname(quantile(df$profit, 0.05, na.rm = TRUE)),
      p95    = unname(quantile(df$profit, 0.95, na.rm = TRUE))
    )
    
    stats_ppn <- c(
      mean   = mean(df$profit_per_night, na.rm = TRUE),
      median = median(df$profit_per_night, na.rm = TRUE),
      sd     = sd(df$profit_per_night, na.rm = TRUE),
      p5     = unname(quantile(df$profit_per_night, 0.05, na.rm = TRUE)),
      p95    = unname(quantile(df$profit_per_night, 0.95, na.rm = TRUE))
    )
    
    stats_pps <- c(
      mean   = mean(df$profit_per_stay, na.rm = TRUE),
      median = median(df$profit_per_stay, na.rm = TRUE),
      sd     = sd(df$profit_per_stay, na.rm = TRUE),
      p5     = unname(quantile(df$profit_per_stay, 0.05, na.rm = TRUE)),
      p95    = unname(quantile(df$profit_per_stay, 0.95, na.rm = TRUE))
    )
    
    cat("Profit over period (USD):\n")
    print(round(stats_profit, 2))
    cat("\nProfit per booked night (USD):\n")
    print(round(stats_ppn, 2))
    cat("\nProfit per stay (USD):\n")
    print(round(stats_pps, 2))
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
