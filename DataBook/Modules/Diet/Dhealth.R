################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Think too much salt can/cannot cause health problems"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dhealth.R"))

dhealth_df <- dhealth(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dhealth_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Think consuming too much salt could cause serious health problem
dhealth_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
dhealth_c <- tbls_list_split(
  .data = dhealth_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) think too much salt can cause health problems")

################################################################################





