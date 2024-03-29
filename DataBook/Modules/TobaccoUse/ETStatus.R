################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "E-cigarettes usage"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "ETStatus.R"))

etstatus_df <- etstatus(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- etstatus_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of current users of e-cigarettes
etstatus_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
etstatus_d <- tbls_list_split(
  .data = etstatus_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) daily and non-daily users")

################################################################################

# E-cigarettes using status
etstatus_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
etstatus_c <- 
  tbls_list_split(.data = etstatus_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################
################################################################################

