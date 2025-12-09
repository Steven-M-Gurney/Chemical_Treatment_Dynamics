###############################################################################
# Decay-Curve Development: Exponential-Decay Models
# Author: Steven M. Gurney
# Last updated: 10 DEC 2025
#
# Purpose:
#   • Fit temperature–half-life exponential decay models for two datasets:
#       (1) Pond-temperature model
#       (2) Lake-and-pond temperature model
#   • Generate plots and decay predictions for scenario modeling
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(ggplot2)
library(dplyr)

# =============================================================================
# 📁 1. Load & Prepare Cross-Study Data
# =============================================================================

df <- read.csv("Cross_Study_Data.csv")

# Inspect dataset (optional)
print(df)

# =============================================================================
# 🧮 2. Function to Fit Exponential Decay Model
# =============================================================================
# Model: half_life = a * exp(b * temp)

fit_decay_model <- function(data_subset, label_suffix) {
  
  message("\n--- Running model: ", label_suffix, " ---")
  
  # Fit nonlinear exponential model
  nls_model <- nls(
    half_life_days ~ a * exp(b * temp_f),
    data  = data_subset,
    start = list(a = 10, b = -0.05)
  )
  
  # Extract parameters
  coef_vals <- coef(nls_model)
  a <- round(coef_vals["a"], 3)
  b <- round(coef_vals["b"], 4)
  
  # Print equations
  cat("Half-life equation:\n")
  cat(paste0("half_life(T) = ", a, " * exp(", b, " * T)\n\n"))
  
  cat("Decay-rate (k) equation:\n")
  cat(paste0("k(T) = log(2) / (", a, " * exp(", b, " * T))\n"))
  
  # Generate prediction curve
  temp_range <- data.frame(temp_f = seq(30, 80, length.out = 300))
  temp_range$predicted_half_life <- predict(nls_model, newdata = temp_range)
  
  # Equation label for plot
  eq_label <- bquote(k(t) == frac(ln(2), .(a) * e^{.(b) * t}))
  
  # Plot
  plot_out <- ggplot() +
    geom_point(data = data_subset,
               aes(x = temp_f, y = half_life_days),
               size = 4, color = "grey20") +
    geom_line(data = temp_range,
              aes(x = temp_f, y = predicted_half_life),
              color = "violetred4", size = 2) +
    annotate("text",
             x = 60, y = max(data_subset$half_life_days, na.rm = TRUE),
             label = as.character(as.expression(eq_label)),
             parse = TRUE, hjust = 0, size = 5) +
    labs(
      x = "Water temperature (°F)",
      y = "Half-life (days)",
      title = paste0("Exponential Decay Model: ", label_suffix)
    ) +
    theme_classic() +
    theme(
      axis.title.x = element_text(face = "bold", size = 20),
      axis.title.y = element_text(face = "bold", size = 20),
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12)
    )
  
  # Save plot, leading with "HalfLife_" in the filename.
  ggsave(
    paste0("HalfLife_", label_suffix, ".jpeg"),
    plot   = plot_out,
    width  = 6.5,
    height = 6.5,
    dpi    = 300
  )
  
  # Prediction helper function
  predict_half_life <- function(temp_f) {
    predict(nls_model, newdata = data.frame(temp_f = temp_f))
  }
  
  return(list(
    model = nls_model,
    a = a,
    b = b,
    plot = plot_out,
    predict_half_life = predict_half_life
  ))
}

# =============================================================================
# 🔁 3. Run the Two Candidate Models
# =============================================================================

# Pond-only model
pond_data <- df %>%
  filter(waterbody == "Pond") %>%
  select(temp_f, half_life_days)

pond_model <- fit_decay_model(
  data_subset = pond_data,
  label_suffix = "Pond-temperature model"
)

# Lake + Pond model
lake_pond_data <- df %>%
  filter(waterbody %in% c("Pond", "Lake")) %>%
  select(temp_f, half_life_days)

lake_pond_model <- fit_decay_model(
  data_subset = lake_pond_data,
  label_suffix = "Lake-and-pond temperature model"
)

# =============================================================================
# 🔮 4. Example Predictions
# =============================================================================

pond_model$predict_half_life(75.2)
lake_pond_model$predict_half_life(75.41)

