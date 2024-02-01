################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Effects of long COVID on ability to carry out day-to-day activities"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COability.R"))

coability_df <- coability(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- coability_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of the population with long-term symptoms that reduce their ability to 
# carry out day-to-day activities compared with the time before they had COVID-19
coability_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
coability_c <- tbls_list_split(
  .data = coability_c_list_long, .select_var = c, .vars_amount_number = 3)
# m1=Yes, a lot
# m2=Yes, a little
# m3=Not at all
################################################################################

