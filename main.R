library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)
library(openxlsx2)

# ==============================================================================
# Configuration

current_year <- 2025
percentiles <- seq(0.05, 0.95, by = 0.05)  # 5th to 95th percentiles

# Variables to extract from ORG data
var_list <- c("year", "month", "age", "selfemp", "selfinc", "emp", 
              "a_earnhour", "wage", "cow1")

# ==============================================================================
# Load and prepare CPI data

cpi_year <- c_cpi_u_extended_annual

# ==============================================================================
# Calculate wage percentiles for comparison periods

wages_percentiles <- load_org(
  c(1979, current_year), # Load only needed years
  all_of(c(var_list, "orgwgt", "finalwgt"))
) |>
  filter(age >= 16, selfemp == 0, wage > 0) |>
  left_join(cpi_year, by = "year") |>
  mutate(
    # Get 1979 CPI value
    cpi_2025 = c_cpi_u_extended[year == 2025][1],
    real_wage = wage * (cpi_2025 / c_cpi_u_extended)
  ) |>
  reframe(
    percentile = percentiles,
    earnings = averaged_quantile(
      x = real_wage,
      w = orgwgt / 12,
      probs = percentiles
    ) * 2080,
    .by = year
  )
# ==============================================================================
# Data Frame output for calculator

wages_calculator <- wages_percentiles |>
  pivot_wider(
    names_from = year,
    values_from = earnings
  ) |>
  rename(
    actual = `2025`,
    potential = `1979`       
  ) |> 
  # Apply productivity adjustment factor (87.3% growth since 1979)
  mutate(potential = potential * 1.873)

wb_workbook() |> wb_add_worksheet("Wage Calculator") |> 
  wb_add_data(x = wages_calculator) |>
  wb_save("wage_calculator/output/wage_calculator.xlsx", overwrite = TRUE)