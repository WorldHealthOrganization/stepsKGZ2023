################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Healthcare services use for long-term COVID symptoms"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "Covid", "functions", "COservice.R"))

coservice_df <- coservice(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- coservice_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage out of the general population with symptoms of Long COVID
# who utilized healthcare services:
# a. Primary healthcare services
coservice_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = co6acln)

# DATABOOK prep
coservice_a <- tbls_list_split(
  .data = coservice_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "1) utilized")

#-------------------------------------------------------------------------------
# b. Specialized healthcare services (e.g., hospital)
coservice_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = co6bcln)

# DATABOOK prep
coservice_b <- tbls_list_split(
  .data = coservice_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "1) utilized")

#-------------------------------------------------------------------------------
# c. Other healthcare services
coservice_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = co6ccln)

# DATABOOK prep
coservice_c <- tbls_list_split(
  .data = coservice_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) utilized")

################################################################################




