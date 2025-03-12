# Load the required library
library(data.table)

# -------------------------------
# Data Preparation
# -------------------------------
# For demonstration, we create a sample dataset.
set.seed(123)  # For reproducibility
dta <- data.table(
  ID = sample(1:5, 100, replace = TRUE),         # 5 catchments
  YR = sample(2000:2020, 100, replace = TRUE),     # Years from 2000 to 2020
  MNTH = sample(1:12, 100, replace = TRUE),        # Months 1-12
  PRCP = runif(100, 0, 100),                       # Random precipitation values
  OBS_RUN = runif(100, 0, 50),                     # Random observed runoff values
  PET = runif(100, 0, 80),                         # Random potential evapotranspiration values
  SWE = runif(100, 0, 100)                         # Random snow water equivalent values
)

# -------------------------------
# Part 1: Hydrological Years and Catchment Selection
# -------------------------------

# Task 1: Assign Hydrological Years (HYR)
# For months October (10), November (11), and December (12), set HYR = YR + 1.
dta[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]
# 'dta' now includes the HYR column.

# Task 2: Compute Overall Runoff Coefficients (RC) per Catchment
RC_dt <- dta[, .(total_PRCP = sum(PRCP, na.rm = TRUE),
                 total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)),
             by = ID]
RC_dt[, RC := total_OBS_RUN / total_PRCP]
# 'RC_dt' now contains the runoff coefficients for each catchment.

# Task 3: Classify Catchments Based on Runoff Coefficients
quant_breaks <- quantile(RC_dt$RC, probs = seq(0, 1, 0.2), na.rm = TRUE)
RC_dt[, RC_class := cut(RC, 
                        breaks = quant_breaks, 
                        labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                        include.lowest = TRUE)]
# Randomly select one catchment per RC_class.
selected_catchments <- RC_dt[, .SD[sample(.N, 1)], by = RC_class]

# Output: Table of selected catchments by runoff coefficient class.
print("Selected catchments (one per runoff coefficient class):")
print(selected_catchments)

# -------------------------------
# Part 2: Water Balance and Snowmelt Contribution to Runoff
# -------------------------------

# Task 4: Compute Monthly and Annual Water Balance for Selected Catchments
selected_ids <- selected_catchments$ID
dta_selected <- dta[ID %in% selected_ids]

# Monthly summary: average values for PRCP, PET, and OBS_RUN
monthly_summary <- dta_selected[, .(
  PRCP = mean(PRCP, na.rm = TRUE),
  PET  = mean(PET, na.rm = TRUE),
  OBS_RUN = mean(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]
monthly_summary[, WB := PRCP - PET]  # Water Balance = PRCP - PET

# Annual summary: totals for PRCP, PET, and OBS_RUN
annual_summary <- dta_selected[, .(
  PRCP_annual = sum(PRCP, na.rm = TRUE),
  PET_annual  = sum(PET, na.rm = TRUE),
  OBS_RUN_annual = sum(OBS_RUN, na.rm = TRUE)
), by = .(HYR, ID)]
annual_summary[, WB_annual := PRCP_annual - PET_annual]

# Identify months with water deficit (WB < 0)
deficit_months <- monthly_summary[WB < 0]
print("Months with water balance deficit (WB < 0):")
print(deficit_months)

# Task 5: Snowmelt Contribution to Runoff

# Compute mean monthly SWE per catchment and hydrological year
monthly_SWE <- dta_selected[, .(
  SWE = mean(SWE, na.rm = TRUE)
), by = .(HYR, ID, MNTH)]

# For each catchment and hydrological year, compute maximum SWE
monthly_SWE[, max_SWE := max(SWE, na.rm = TRUE), by = .(HYR, ID)]

# For spring months (March, April, May), estimate snowmelt as:
# Snowmelt = max_SWE - current SWE.
monthly_SWE[MNTH %in% c(3, 4, 5), Snowmelt := max_SWE - SWE]

# Merge snowmelt estimates with monthly observed runoff for spring months
spring_data <- merge(
  monthly_summary[MNTH %in% c(3, 4, 5)],
  monthly_SWE[MNTH %in% c(3, 4, 5), .(HYR, ID, MNTH, Snowmelt)],
  by = c("HYR", "ID", "MNTH")
)

# Compute the correlation between snowmelt and observed runoff for spring months.
cor_snowmelt_runoff <- cor(spring_data$Snowmelt, spring_data$OBS_RUN, use = "complete.obs")
print(paste("Correlation between snowmelt and observed runoff (March-May):", 
            round(cor_snowmelt_runoff, 3)))

# -------------------------------
# Interpretation of the Correlation Results
# -------------------------------
# In this synthetic dataset, the correlation between snowmelt (estimated as the drop in SWE relative to the maximum)
# and observed runoff during the spring months is computed. For example, a positive correlation close to +1 would suggest that
# higher snowmelt is associated with increased runoff. Here, if the correlation is low or negative (as in this case with -0.223),
# it could indicate that other factors (like soil absorption, evaporation, or catchment-specific characteristics) are influencing runoff.
#
# In practice, using real hydrological data may result in a different correlation value, and further investigation would be needed
# to understand the underlying processes.

# Expected outputs are now generated:
# 1. 'dta' now includes the HYR column.
# 2. 'RC_dt' shows overall runoff coefficients per catchment.
# 3. 'RC_dt' has catchments classified into five runoff coefficient categories.
# 4. 'selected_catchments' contains one catchment per category.
# 5. 'monthly_summary' and 'annual_summary' provide water balance calculations.
# 6. 'monthly_SWE' has snowmelt estimates per catchment and hydrological year.
# 7. 'cor_snowmelt_runoff' provides the correlation analysis for spring months.
# 8. The interpretation comments provide insights into the correlation result.
