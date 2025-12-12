###############################################################################
# Estimation of Pond 3E Site Conditions
# Author: Steven M. Gurney
# Last updated: 10 DEC 2025
#
# Purpose:
#   • Summarize temperature and pH measurements from WCAA Environmental logbooks
#   • Visualize temporal patterns in water-temperature sampling
#   • Generate monthly and summer-period summary statistics
#   • Export summary tables for modeling inputs
###############################################################################

# 📦 Load Required Packages ----------------------------------------------------
library(dplyr)
library(ggplot2)

# =============================================================================
# 📁 1. Load & Prepare Logbook Data
# =============================================================================

# Read in pond data (derived from WCAA Environmental logbooks)
dat <- read.csv("Pond_Data_Environmental.csv")   # Note: column names standardized prior

# Look at number of records by pond
print(table(dat$Pond)) # Pond 3E = 40; 3W = 1; 4 = 3; 6 = 25

# Assign Date and Month fields
dat <- dat %>%
  mutate(
    Date  = as.Date(Date, format = "%m/%d/%Y"),
    Month = format(Date, "%B")
  )

# Restrict to Pond 3E only
dat <- dat %>% filter(Pond == "3E")

# Clean and convert time variable to POSIX time format (24 hr)
dat <- dat %>%
  mutate(
    Time_str    = sprintf("%04d", as.integer(Time_24.hr)),               # ensure HHMM
    Time_parse  = strptime(Time_str, format = "%H%M") %>% format("%H:%M:%S"),
    Time_parsed = as.POSIXct(Time_parse, format = "%H:%M:%S")
  )

# =============================================================================
# 📊 2. Density Plot of Sampling Times
# =============================================================================

dp <- ggplot(dat, aes(x = Time_parsed)) +
  geom_density(fill = "antiquewhite3", alpha = 0.6) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  labs(
    x = "Time of day (24 hr)",
    y = "Sampling density",
  ) +
  theme_classic()+
  theme(
    axis.title = element_text(face = "bold", size = 16),  # Bold, larger axis titles
    axis.text  = element_text(size = 12),                  # Slightly smaller axis labels
  )

# Save plot
ggsave("TempDensity.jpeg", plot = dp, width = 6.5, height = 6.5, dpi = 300)

# =============================================================================
# 📋 3. Select Key Variables and Convert Temperature Units
# =============================================================================

dat <- dat %>%
  select(Pond, Month, Water.temp_C, pH) %>%
  mutate(
    Water.temp_F = as.numeric(Water.temp_C) * 9/5 + 32   # °F conversion
  )

# Take a look at the data
print(dat)

# =============================================================================
# 📆 4. Summary Statistics by Month
# =============================================================================

summary_table <- dat %>%
  group_by(Month) %>%
  summarise(
    WaterTempC_Min = min(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Max = max(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Avg = mean(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_SD  = sd(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempF_Min = min(Water.temp_F, na.rm = TRUE),
    WaterTempF_Max = max(Water.temp_F, na.rm = TRUE),
    WaterTempF_Avg = mean(Water.temp_F, na.rm = TRUE),
    WaterTempF_SD  = sd(Water.temp_F, na.rm = TRUE),
    pH_Min         = min(as.numeric(pH), na.rm = TRUE),
    pH_Max         = max(as.numeric(pH), na.rm = TRUE),
    pH_Avg         = mean(as.numeric(pH), na.rm = TRUE),
    pH_SD          = sd(as.numeric(pH), na.rm = TRUE)
  )

View(summary_table)

# Sample sizes by month
dat %>% count(Month) # 9 records for June; 7 for Septemeber

# =============================================================================
# 🌡️ 5. Restrict to Peak Summer Months (July & August)
# =============================================================================

# Use only peak heat months
dat_summer <- dat %>% filter(Month %in% c("July", "August"))

# Count number of July and August records
dat_summer %>% count()   # 24 records

# Recalculate summary for summer period
summary_table_summer <- dat_summer %>%
  summarise(
    WaterTempC_Min = min(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Max = max(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Avg = mean(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_SD  = sd(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempF_Min = min(Water.temp_F, na.rm = TRUE),
    WaterTempF_Max = max(Water.temp_F, na.rm = TRUE),
    WaterTempF_Avg = mean(Water.temp_F, na.rm = TRUE),
    WaterTempF_SD  = sd(Water.temp_F, na.rm = TRUE),
    pH_Min         = min(as.numeric(pH), na.rm = TRUE),
    pH_Max         = max(as.numeric(pH), na.rm = TRUE),
    pH_Avg         = mean(as.numeric(pH), na.rm = TRUE),
    pH_SD          = sd(as.numeric(pH), na.rm = TRUE)
  )


print(summary_table_summer)

# =============================================================================
# 💾 6. Save Monthly & Summer Summary Tables
# =============================================================================

write.csv(summary_table,        "Pond3E_Summary_ByMonth.csv", row.names = FALSE)
write.csv(summary_table_summer, "Pond3E_Summary_JulyAugust.csv", row.names = FALSE)


