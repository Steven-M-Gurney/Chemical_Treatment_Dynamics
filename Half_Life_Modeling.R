###############################################################################
# Chemical-Dynamics Modeling of Rotenone Persistence
# Author: Steven M. Gurney
# Last updated: 10 DEC 2025
#
# Purpose:
#   • Fit exponential-decay and log–linear models for rotenone half-life
#   • Generate predictions under site-relevant conditions
#   • Integrate results via model averaging for management interpretation
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(ggplot2)
library(dplyr)

# =============================================================================
# 🎨 Global Plot Color Definitions
# =============================================================================

# Exponential models
exp_colors <- c(
  pond      = "darkseagreen4",
  lakepond  = "darkseagreen3"
)

# Log–linear models
colors_full_loglinear <- c(
  "Temperature (°F)" = "palevioletred3",
  "pH"               = "steelblue3"
)

colors_restricted_loglinear <- c(
  "Temperature (°F)" = "palevioletred4",
  "pH"               = "steelblue4"
)



# =============================================================================
# 📁 1. Load & Inspect Cross-Study Data
# =============================================================================

df <- read.csv("Cross_Study_Data.csv") # Manually created during lit review
print(summary(df))



# =============================================================================
# 🧮 2. Exponential-Decay Model Function
# =============================================================================

# Model form (Equation 3)
# half_life = a * exp(b * temp) 

fit_decay_model <- function(data_subset, label_suffix, line_color) {
  
  # Add identifier message based on model 
  message("\n--- Running model: ", label_suffix, " ---")
  
  # Fit nonlinear exponential model
  nls_model <- nls(
    half_life_days ~ a * exp(b * temp_f),
    data  = data_subset,
    start = list(a = 10, b = -0.05)
  )
  
  # Extract parameters
  coef_vals <- coef(nls_model)
  a <- unname(round(coef_vals["a"], 3))
  b <- unname(round(coef_vals["b"], 4))
  
  # Generate prediction curve
  temp_range <- data.frame(temp_f = seq(30, 80, length.out = 300))
  temp_range$predicted_half_life <- predict(nls_model, newdata = temp_range)
  
  # Equation label for plot
  eq_label <- bquote(
    italic(k)(italic(T)) == frac(ln(2), .(a) * e^{.(b) * italic(T)})
  )
  
  # Plot results
  plot_out <- ggplot() +
    geom_point(data = data_subset,
               aes(x = temp_f, y = half_life_days),
               size = 4, color = "grey20") +
    geom_line(data = temp_range,
              aes(x = temp_f, y = predicted_half_life),
              color = line_color, size = 2) +
    annotate("text",
             x = 50, y = 10,
             label = paste(deparse(eq_label), collapse = ""),
             parse = TRUE, hjust = 0, size = 8) +
    labs(
      x = "Water temperature (°F)",
      y = "Half-life (days)",
      title = paste0("Exponential Decay Model: ", label_suffix)
    ) +
    theme_classic()
  
  # Save plot, leading with "HalfLife_" in the filename and adding defined label
  ggsave(
    paste0("HalfLife_", label_suffix, ".jpeg"),
    plot = plot_out,
    width = 6.5,
    height = 6.5,
    dpi = 300
  )
  
  # Prediction helper function
  predict_half_life <- function(temp_f) {
    predict(nls_model, newdata = data.frame(temp_f = temp_f))
  }
  
  # Kick out results
  return(list(
    model = nls_model,
    a = a,
    b = b,
    plot = plot_out,
    predict = predict_half_life
  ))
}



# =============================================================================
# 🔁 3. Fit Exponential Models
# =============================================================================

# Pond-only model
pond_exp <- fit_decay_model(
  data_subset = df %>% filter(waterbody == "Pond") %>% select(temp_f, half_life_days),
  label_suffix = "Pond-temperature model",
  line_color = exp_colors["pond"]
)

# Lake + Pond model
lake_pond_exp <- fit_decay_model(
  data_subset = df %>% filter(waterbody %in% c("Pond", "Lake")) %>% select(temp_f, half_life_days),
  label_suffix = "Lake-and-pond temperature model",
  line_color = exp_colors["lakepond"]
)



# =============================================================================
# 🧮 4. Log–Linear Model Function
# =============================================================================

# Model form (Equations 5 and 6):
#   log(t_half) = β0 + β1 * temp_f + β2 * pH + ε
#   t_half_hat  = exp(β0 + β1 * temp_f + β2 * pH)

fit_log_linear_model <- function(data_subset, label_suffix, line_colors) {
  
  # Add identifier message based on model 
  message("\n--- Running log–linear model: ", label_suffix, " ---")
  
  # Fit model and summarize results
  log_model <- lm(log(half_life_days) ~ temp_f + ph, data = data_subset)
  print(summary(log_model))
  
  # Extract coefficients
  betas <- coef(log_model)
  
  # Prediction function
  predict_half_life <- function(temp_f, ph) {
    exp(predict(log_model, newdata = data.frame(temp_f = temp_f, ph = ph)))
  }
  
  # Build predictor ranges
  temp_seq <- seq(min(data_subset$temp_f, na.rm = TRUE),
                  max(data_subset$temp_f, na.rm = TRUE), length.out = 100)
  ph_seq   <- seq(min(data_subset$ph, na.rm = TRUE),
                  max(data_subset$ph, na.rm = TRUE), length.out = 100)
  
  # Average temp and pH
  avg_temp <- mean(data_subset$temp_f, na.rm = TRUE)
  avg_ph   <- mean(data_subset$ph, na.rm = TRUE)
  
  # Temperature and pH effects
  temp_effect <- data.frame(temp_f = temp_seq, ph = avg_ph)
  ph_effect   <- data.frame(temp_f = avg_temp, ph = ph_seq)
  
  temp_preds <- predict(log_model, newdata = temp_effect, se.fit = TRUE)
  ph_preds   <- predict(log_model, newdata = ph_effect, se.fit = TRUE)
  
  combined_effects <- bind_rows(
    temp_effect %>%
      mutate(Predictor = "Temperature (°F)",
             Value = temp_f,
             predicted_half_life = exp(temp_preds$fit),
             lower = exp(temp_preds$fit - 1.96 * temp_preds$se.fit),
             upper = exp(temp_preds$fit + 1.96 * temp_preds$se.fit)),
    ph_effect %>%
      mutate(Predictor = "pH",
             Value = ph,
             predicted_half_life = exp(ph_preds$fit),
             lower = exp(ph_preds$fit - 1.96 * ph_preds$se.fit),
             upper = exp(ph_preds$fit + 1.96 * ph_preds$se.fit))
  )
  
  # Plot results
  plot_out <- ggplot(combined_effects,
                     aes(x = Value, y = predicted_half_life)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "gray80", alpha = 0.5) +
    geom_line(aes(color = Predictor), size = 2) +
    scale_color_manual(values = line_colors) +
    facet_wrap(~ Predictor, scales = "free_x") +
    labs(x = NULL, y = "Half-life (days)") +
    theme_classic() +
    theme(legend.position = "none")
  
  # Save plot, leading with "HalfLife_" in the filename and adding defined label
  ggsave(
    paste0("HalfLife_", gsub(" ", "_", label_suffix), ".jpeg"),
    plot = plot_out,
    width = 6.5,
    height = 6.5,
    dpi = 300
  )
  
  # Kick out results
  return(list(
    model = log_model,
    betas = betas,
    plot = plot_out,
    predict = predict_half_life
  ))
}



# =============================================================================
# 🔁 5. Fit Log–Linear Models
# =============================================================================

# Full temperature-and-pH model
full_log <- fit_log_linear_model(
  data_subset = df %>% filter(!is.na(ph), !is.na(temp_f), !is.na(half_life_days)),
  label_suffix = "Temperature-and-pH Model",
  line_colors = colors_full_loglinear
)

# Restricted temperature-and-pH model
restricted_log <- fit_log_linear_model(
  data_subset = df %>% filter(temp_f > 40, ph > 7, ph < 9, !is.na(half_life_days)),
  label_suffix = "Restricted Temperature-and-pH Model",
  line_colors = colors_restricted_loglinear
)



# =============================================================================
# 🔮 6. Generate Predictions for Site Conditions
# =============================================================================

# Average site temp and pH estimated from in-house data
target_temp <- 75.41
target_ph   <- 7.99

# All four candidate-model predictions
pred_exp_pond       <- pond_exp$predict(target_temp)
pred_exp_lakepond   <- lake_pond_exp$predict(target_temp)
pred_log_full       <- full_log$predict(target_temp, target_ph)
pred_log_restricted <- restricted_log$predict(target_temp, target_ph)

# Create table
prediction_table <- data.frame(
  Model = c(
    "Pond-temperature exponential",
    "Lake-and-pond exponential",
    "Full temperature–pH log–linear",
    "Restricted temperature–pH log–linear"
  ),
  HalfLife_Days = c(
    pred_exp_pond,
    pred_exp_lakepond,
    pred_log_full,
    pred_log_restricted
  )
)

# Half-life estimates by model
print(prediction_table)



# =============================================================================
# 📊 7. Model Averaging Across All Four Models
# =============================================================================

# Summary statistics
mean_half_life  <- mean(prediction_table$HalfLife_Days)
sd_half_life    <- sd(prediction_table$HalfLife_Days)
min_half_life   <- min(prediction_table$HalfLife_Days)
max_half_life   <- max(prediction_table$HalfLife_Days)
range_half_life <- max_half_life - min_half_life

# Print summary statistics
cat("\n================ MODEL-AVERAGED RESULTS ================\n")
cat("Mean half-life (days):        ", round(mean_half_life, 3), "\n")
cat("Between-model SD (days):      ", round(sd_half_life, 3), "\n")
cat("Min predicted half-life:      ", round(min_half_life, 3), "\n")
cat("Max predicted half-life:      ", round(max_half_life, 3), "\n")
cat("Range (max - min):            ", round(range_half_life, 3), "\n")
cat("========================================================\n\n")

# Update summary table
prediction_table <- prediction_table %>%
  add_row(
    Model = "Model-averaged estimate",
    HalfLife_Days = mean_half_life
  )

print(prediction_table)



# =============================================================================
# =============================================================================
# 📉 8. Model-Averaged Decay Curve (Relative Concentration Over Time)
# =============================================================================
#   • Visualize expected rotenone decay under average DTW site conditions
#   • Uses model-averaged half-life and first-order kinetics
#   • Expressed as relative concentration (C(t) / C0)
# =============================================================================

# Convert model-averaged half-life to decay rate (Equation 4)
k_avg <- log(2) / mean_half_life

# Time sequence for decay visualization (days)
time_df <- data.frame(
  time_days = seq(0, 10, length.out = 300)
)

# First-order decay using model-averaged k
time_df$rel_conc <- exp(-k_avg * time_df$time_days)

# Convert model-averaged half-life to decay rate (Equation 4)
k_avg <- log(2) / mean_half_life

# Time sequence for decay visualization (days)
time_df <- data.frame(
  time_days = seq(0, 10, length.out = 300)
)

# First-order decay using model-averaged k
time_df$rel_conc <- exp(-k_avg * time_df$time_days)

# Plot 
avg_decay_plot <- ggplot(time_df,
                         aes(x = time_days, y = rel_conc)) +
    # Half-life reference line (add first so behind curve line)
  geom_vline(
    xintercept = mean_half_life,
    linetype = "dashed",
    linewidth = 1,
    color = "grey"
  ) +
    # Decay curve
  geom_line(color = "firebrick", size = 2) +
    # Annotation for half-life
  annotate(
    "text",
    x = 5.2,
    y = 0.52,
    label = paste0(
      "Model-averaged half-life\n= ",
      round(mean_half_life, 2), " days"
    ),
    hjust = 0.5,
    vjust = 0.5,
    size = 5
  ) +
  
  labs(
    x = "Time since treatment (days)",
    y = "Relative concentration"
  ) +
  
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    plot.title   = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14)
  )

# Look at plot
avg_decay_plot 

# Save output
ggsave(
  "Rotenone_ModelAveraged_DecayCurve.jpeg",
  plot   = avg_decay_plot,
  width  = 6.5,
  height = 4,
  dpi    = 300
)
