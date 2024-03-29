################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Thinking about quitting due to warnings on cig packages"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPquitting.R"))

tpquitting_df <- tpquitting(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpquitting_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current smokers who saw health warnings on cigarette packages that thought of quitting
tpquitting_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
tpquitting_c <- tbls_list_split(
  .data = tpquitting_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) thought about quitting")

