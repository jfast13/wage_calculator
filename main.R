library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)

# ==============================================================================
# Configuration

current_year <- 2025
percentiles <- seq(0.05, 0.95, by = 0.05)  # 5th to 95th percentiles

# Variables to extract from ORG data
var_list <- c("year", "month", "age", "selfemp", "selfinc", "emp", 
              "a_earnhour", "wage", "cow1")

# ==============================================================================
# Load and prepare CPI data

cpi_monthly <- c_cpi_u_extended_monthly_nsa |> 
  rename(c_cpi_u_month = c_cpi_u_extended)

cpi_year <- c_cpi_u_extended_annual

# ==============================================================================
# Calculate wage percentiles for comparison periods

wages_percentiles <- load_org(
  c(1979, (current_year - 1):current_year),  # Load only needed years
  all_of(c(var_list, "orgwgt", "finalwgt"))
) |>
  filter(age >= 16, selfemp == 0, wage > 0) |>
  
  # Define comparison periods: 1979 baseline vs. most recent 12 months
  mutate(
    period = case_when(
      year == 1979 ~ "1979",
      (year == current_year - 1 & month > max(month[year == current_year])) |
      (year == current_year & month <= max(month[year == current_year])) ~ 
        "Last 12 Months",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(period)) |>  # Keep only comparison periods
  
  # Join CPI data for inflation adjustment
  left_join(cpi_monthly, by = c("year", "month")) |>
  left_join(cpi_year, by = "year") |>
  
  # Calculate reference CPI (average of most recent 12 months)
  mutate(cpi_recent = mean(c_cpi_u_month[period == "Last 12 Months"], 
                           na.rm = TRUE)) |>
  
  mutate(
    real_wage = if_else(
      period == "1979",
      wage * (cpi_recent / c_cpi_u_extended),  # Inflate 1979 wages
      wage  # Recent wages need no adjustment
    )
  ) |>
  
  # Convert hourly wages to annual (2080 hours = 40 hrs/week * 52 weeks)
  reframe(
    percentile = percentiles,
    earnings = averaged_quantile(
      x = real_wage,
      w = orgwgt / 12,  
      probs = percentiles
    ) * 2080,  
    .by = period
  )

# ==============================================================================
# Data Frame output for calculator

wages_calculator <- wages_percentiles |>
  pivot_wider(
    names_from = period,
    values_from = earnings
  ) |>
  rename(
    actual = `Last 12 Months`,
    potential = `1979`       
  ) |> 
  # Apply productivity adjustment factor (90.2% growth since 1979)
  mutate(potential = potential * 1.902)

write.csv(wages_calculator, "output/wage_calculator_values.csv", row.names = FALSE)