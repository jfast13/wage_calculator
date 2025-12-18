library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)
library(openxlsx2)

# ==============================================================================
# Configuration

current_year <- 2025
percentiles <- 0.5 

# Variables to extract from ORG data
var_list <- c("year", "month", "age", "selfemp", "selfinc", "emp", "statefips",
              "a_earnhour", "wage", "cow1")

# ==============================================================================
# Load and prepare CPI data

cpi_year <- c_cpi_u_extended_annual
cpi_current <-  cpi_year$c_cpi_u_extended[cpi_year$year == c(current_year-1)]

# ==============================================================================
# Calculate wage percentiles for comparison periods

wages_percentiles_state <- load_org(
  c(1979, (current_year - 3):current_year-1),  # Load only needed years
  all_of(c(var_list, "orgwgt", "finalwgt"))
) |>
  filter(age >= 16, selfemp == 0, wage > 0) |>

  left_join(cpi_year, by = "year") |>
  
  mutate(
    real_wage = wage * (cpi_current / c_cpi_u_extended)
  ) |>
  
  reframe(
    percentile = percentiles,
    earnings = averaged_quantile(
      x = real_wage,
      w = orgwgt / 12,  
      probs = percentiles
    ) * 2080,  
    .by = c(year, statefips)
  ) |> 
pivot_wider(
    names_from = year,
    values_from = earnings,
    names_prefix = "earnings_"
  ) |>
   rowwise() |>
  mutate(
    earnings_variation = sd(c_across(starts_with("earnings_")), na.rm = TRUE), 
    potential_1979 = earnings_1979 *1.873
  ) |>
  ungroup()

write_xlsx(wages_percentiles_state, "state_data.xlsx")
  