# Load packages
library(dplyr)
library(ggplot2)

# Read in pond data (derived from WCAA Environmental logbooks).
dat <- read.csv("Pond_Data_Environmental.csv") # Had to revise column names prior to this step.

# Look at number of records records by pond
print(table(dat$Pond))

# Assign Month
dat <- dat %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Format date
    Month = format(Date, "%B")                  # Extract month name
  )

# Use only Pond 3E data
dat <- dat %>%
  filter(Pond == "3E")

# Clean and convert Time_24.hr to a proper time format (e.g., "HH:MM")
dat <- dat %>%
  mutate(
    Time_str = sprintf("%04d", as.integer(Time_24.hr)),              # Ensure 4-digit string
    Time_parsed = strptime(Time_str, format = "%H%M") %>% format("%H:%M:%S"), # Parse to time
    Time_parsed = as.POSIXct(Time_parsed, format = "%H:%M:%S")      # Convert to POSIXct
  )

# Plot density by time.
# Note there is no data collected during peak heat (3-5pm).
dp <- ggplot(dat, aes(x = Time_parsed)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hour") +
  labs(x = "Time of Day", y = "Density", title = "Density of Water-Temp Records") +
  theme_minimal()

# Save plot
ggsave("TempDensity.jpeg", plot = dp, width = 6.5, height = 6.5, dpi = 300)

# Select columns of interest
dat <- dat %>%
  select(Pond, Month, Water.temp_C, pH)

# Take a look
print(dat)

# Convert temperature to Fahrenheit
dat <- dat %>%
  mutate(Water.temp_F = as.numeric(Water.temp_C) * 9/5 + 32)

# Summarize by month
summary_table <- dat %>%
  group_by(Month) %>%
  summarise(
    WaterTempC_Min = min(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Max = max(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Avg = mean(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempF_Min = min(Water.temp_F, na.rm = TRUE),
    WaterTempF_Max = max(Water.temp_F, na.rm = TRUE),
    WaterTempF_Avg = mean(Water.temp_F, na.rm = TRUE),
    pH_Min = min(as.numeric(pH), na.rm = TRUE),
    pH_Max = max(as.numeric(pH), na.rm = TRUE),
    pH_Avg = mean(as.numeric(pH), na.rm = TRUE)
  )

# Look
View(summary_table)

# How many rows of data for each month?
dat %>%
  count(Month) # Address low sample sizes below

# Use only July August
dat <- dat %>%
  filter(Month %in% c("July", "August"))

# How many rows of data for July + August?
dat %>%
  count()

# Convert temperature to Fahrenheit
dat <- dat %>%
  mutate(Water.temp_F = as.numeric(Water.temp_C) * 9/5 + 32)

# Summarize July + August
summary_table <- dat %>%
  summarise(
    WaterTempC_Min = min(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Max = max(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempC_Avg = mean(as.numeric(Water.temp_C), na.rm = TRUE),
    WaterTempF_Min = min(Water.temp_F, na.rm = TRUE),
    WaterTempF_Max = max(Water.temp_F, na.rm = TRUE),
    WaterTempF_Avg = mean(Water.temp_F, na.rm = TRUE),
    pH_Min = min(as.numeric(pH), na.rm = TRUE),
    pH_Max = max(as.numeric(pH), na.rm = TRUE),
    pH_Avg = mean(as.numeric(pH), na.rm = TRUE)
  )

# Look
print(summary_table) # Use these data for Pond 3E modeling
 

