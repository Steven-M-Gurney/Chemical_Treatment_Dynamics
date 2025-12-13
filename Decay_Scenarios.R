###############################################################################
# Chemical-Dynamics Modeling: Decay + Dilution Scenarios 
# Author: Steven M. Gurney
# Last updated: 13 DEC 2025
#
# Purpose:
#   • Use model-averaged half-life (1.56 days) to evaluate rotenone persistence
#   • Scenario A: time to decay from 200 ppb to <2 ppb (no dilution)
#   • Scenario B: decay 2 days at initial volume, then flood to max volume,
#                 then time to decay to <2 ppb
#   • Summarize results for two example ponds (3W and 3E) and visualize decay
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

# =============================================================================
# 🔧 1. Global Inputs and Assumptions
# =============================================================================

# --- Model inputs (fixed across scenarios) -----------------------------------
half_life_days <- 1.56          # model-averaged half-life (days)
initial_ppb    <- 200           # maximum treatment concentration (ppb)
target_ppb     <- 2             # regulatory threshold (ppb) = 0.002 ppm
decay_days_pre_flood <- 2       # Scenario B: decay duration before pond fills

# --- Pond volumes (million gallons; MG) --------------------------------------
# Both ponds are initially ~3 ft; here we use the volume
# estimates provided for "minimum" and "maximum" volumes.
ponds <- tibble::tibble(
  Pond       = c("Pond 3W", "Pond 3E"),
  Min_vol_MG = c(16.0, 7.5),
  Max_vol_MG = c(66.0, 103.0)
)

# --- Helper for unit conversions (optional) ----------------------------------
ppb_to_ppm <- function(ppb) ppb / 1000
ppm_to_ppb <- function(ppm) ppm * 1000


# =============================================================================
# 🧮 2. Core Chemistry Functions
# =============================================================================

# --- First-order decay using half-life (ppb stays ppb) ------------------------
# C(t) = C0 * (1/2)^(t / half_life)
conc_after_time <- function(initial_ppb, half_life_days, time_days) {
  initial_ppb * (0.5)^(time_days / half_life_days)
}

# --- Time to reach a target concentration using half-life --------------------
# Solve target = initial * (1/2)^(t/HL)  ->  t = log2(initial/target) * HL
time_to_target <- function(initial_ppb, target_ppb, half_life_days) {
  if (target_ppb <= 0) stop("target_ppb must be > 0.")
  if (initial_ppb <= 0) stop("initial_ppb must be > 0.")
  if (initial_ppb <= target_ppb) return(0)
  log2(initial_ppb / target_ppb) * half_life_days
}

# --- Instant dilution via mass balance ---------------------------------------
# Total mass in "ppb * MG" conserved (conceptual scaling), so:
# C_after = C_before * (V_before / V_after)
dilute_to_volume <- function(conc_ppb, vol_before_MG, vol_after_MG) {
  if (vol_before_MG <= 0 | vol_after_MG <= 0) stop("Volumes must be > 0.")
  conc_ppb * (vol_before_MG / vol_after_MG)
}


# =============================================================================
# 🧪 3. Scenario A: Decay Only (No Dilution)
# =============================================================================

time_A_days <- time_to_target(
  initial_ppb    = initial_ppb,
  target_ppb     = target_ppb,
  half_life_days = half_life_days
)

cat("\n================ SCENARIO A: DECAY ONLY ================\n")
cat("Initial concentration (ppb): ", initial_ppb, "\n")
cat("Target threshold (ppb):      ", target_ppb, " (", ppb_to_ppm(target_ppb), "ppm )\n")
cat("Half-life (days):            ", half_life_days, "\n")
cat("Time to reach target:        ", round(time_A_days, 3), " days\n")
cat("========================================================\n\n")


# =============================================================================
# 🌧️ 4. Scenario B: Decay for 2 Days, Then Flood to Max Volume
# =============================================================================
# Step 1: decay for 2 days at initial (minimum) volume (3-ft depth)
# Step 2: instant dilution to max volume
# Step 3: decay from post-flood concentration to threshold

scenario_B_results <- ponds %>%
  rowwise() %>%
  mutate(
    # 1) Concentration after pre-flood decay (still at Min_vol)
    Conc_after_2d_ppb = conc_after_time(
      initial_ppb    = initial_ppb,
      half_life_days = half_life_days,
      time_days      = decay_days_pre_flood
    ),
    
    # 2) Instant dilution to max volume
    Conc_post_flood_ppb = dilute_to_volume(
      conc_ppb        = Conc_after_2d_ppb,
      vol_before_MG   = Min_vol_MG,
      vol_after_MG    = Max_vol_MG
    ),
    
    # 3) Time after flood to reach target
    Days_post_flood_to_target = time_to_target(
      initial_ppb    = Conc_post_flood_ppb,
      target_ppb     = target_ppb,
      half_life_days = half_life_days
    ),
    
    # 4) Total time from treatment start
    Total_days_to_target = decay_days_pre_flood + Days_post_flood_to_target
  ) %>%
  ungroup() %>%
  mutate(
    # Helpful ppm equivalents for reporting
    Conc_after_2d_ppm     = ppb_to_ppm(Conc_after_2d_ppb),
    Conc_post_flood_ppm   = ppb_to_ppm(Conc_post_flood_ppb)
  )

# Display Scenario B table
scenario_B_table <- scenario_B_results %>%
  select(
    Pond,
    Min_vol_MG,
    Max_vol_MG,
    Conc_after_2d_ppb,
    Conc_post_flood_ppb,
    Days_post_flood_to_target,
    Total_days_to_target
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

kable(scenario_B_table, align = "c",
      col.names = c(
        "Pond",
        "Min volume (MG)",
        "Max volume (MG)",
        "Conc after 2 d (ppb)",
        "Conc post-flood (ppb)",
        "Days post-flood to <2 ppb",
        "Total days to <2 ppb"
      )) %>%
  kable_styling(full_width = FALSE, font_size = 12)

# Optional: write CSV for appendix / reproducibility
write.csv(scenario_B_table, "ScenarioB_DecayDilution_Results.csv", row.names = FALSE)


# =============================================================================
# 📉 5. Visualization: Scenario B Decay + Dilution with Regulatory Threshold
# =============================================================================
#  • Shows decay at minimum volume, instant dilution, then continued decay
#  • Includes regulatory threshold (<2 ppb)
#  • Marks when each pond crosses the threshold

# Build piecewise decay curve for each pond
build_piecewise_curve <- function(pond_name, min_vol, max_vol,
                                  initial_ppb, half_life_days,
                                  t_flood_days = 2, t_max_days = 10) {
  
  # --- Pre-flood decay ---
  t1 <- seq(0, t_flood_days, length.out = 150)
  c1 <- conc_after_time(initial_ppb, half_life_days, t1)
  
  # Concentration immediately before flood
  c_pre_flood_end <- tail(c1, 1)
  
  # --- Instant dilution at flood ---
  c_post_flood_start <- dilute_to_volume(c_pre_flood_end, min_vol, max_vol)
  
  # --- Post-flood decay ---
  t2 <- seq(0, max(0, t_max_days - t_flood_days), length.out = 200)
  c2 <- conc_after_time(c_post_flood_start, half_life_days, t2)
  
  tibble::tibble(
    Pond = pond_name,
    time_days = c(t1, t_flood_days + t2),
    conc_ppb  = c(c1, c2)
  )
}

# Build curves for both ponds
curve_df <- scenario_B_results %>%
  rowwise() %>%
  do(build_piecewise_curve(
    pond_name      = .$Pond,
    min_vol        = .$Min_vol_MG,
    max_vol        = .$Max_vol_MG,
    initial_ppb    = initial_ppb,
    half_life_days = half_life_days,
    t_flood_days   = decay_days_pre_flood,
    t_max_days     = 10
  )) %>%
  ungroup()

# Calculate threshold-crossing times for annotation
threshold_points <- scenario_B_results %>%
  transmute(
    Pond,
    time_days = Total_days_to_target,
    conc_ppb  = target_ppb
  )

# Add label text for threshold annotation
threshold_points <- threshold_points %>%
  mutate(
    label = paste0(
      "Threshold reached at\n",
      round(time_days, 2), " days"
    )
  )

# Plot
plot_B <- ggplot(curve_df, aes(x = time_days, y = conc_ppb)) +

  # Flood timing
  geom_vline(
    xintercept = decay_days_pre_flood,
    linetype = "dotted",
    linewidth = 0.75,
    color = "grey50"
  ) +

  # Regulatory threshold
  geom_hline(
    yintercept = target_ppb,
    linetype = "dotted",
    linewidth = 0.75,
    color = "grey50"
  ) +
  
  # Decay curves (do last so plotted over other lines)
  geom_line(color = "steelblue", linewidth = 1.5) +
  
  # Threshold crossing points
  geom_point(
    data = threshold_points,
    aes(x = time_days, y = conc_ppb),
    size = 5,
    shape = 4,
    stroke = 2,
    color = "firebrick"
  ) +
  
  # Centered annotation for time to threshold (shifted right and up)
  geom_text(
    data = threshold_points,
    aes(
      x = time_days + 1.2,      # shift RIGHT (increase if needed)
      y = conc_ppb * 20,      # shift UP (multiplicative works better across facets)
      label = label
    ),
    hjust = 0.5,
    vjust = 0.5,
    size = 4,
    color = "black"
  ) +
  
  facet_wrap(~ Pond, scales = "free_y") +
  
  labs(
    x = "Time since treatment (days)",
    y = "Rotenone concentration (ppb)",
    ) +
  
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    strip.text   = element_text(face = "bold", size = 14),
    plot.title   = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )

# View plot
plot_B

# Save output
ggsave(
  "Rotenone_ScenarioB_DecayDilutionCurves_withThreshold.jpeg",
  plot   = plot_B,
  width  = 9,
  height = 4,
  dpi    = 300
)


# =============================================================================
# ✅ 6. Quick Combined Summary (Scenario A vs Scenario B)
# =============================================================================

summary_out <- scenario_B_results %>%
  transmute(
    Pond,
    ScenarioA_Days_to_2ppb = round(time_A_days, 3),
    ScenarioB_TotalDays_to_2ppb = round(Total_days_to_target, 3),
    PostFlood_Conc_ppb = round(Conc_post_flood_ppb, 3)
  )

kable(
  summary_out,
  align = "c",
  col.names = c(
    "Pond",
    "Scenario A: Days to <2 ppb (decay only)",
    "Scenario B: Total days to <2 ppb (decay + flood)",
    "Post-flood concentration (ppb)"
  )
) %>%
  kable_styling(full_width = FALSE, font_size = 12)

# Save as csv
write.csv(
  summary_out,
  "ScenarioA_vs_ScenarioB_Summary.csv",
  row.names = FALSE
)










