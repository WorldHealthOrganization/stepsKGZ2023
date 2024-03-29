################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Adding salt while cooking"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dcooking.R"))

dcooking_df <- dcooking(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dcooking_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Add salt always or often before eating or when eating 
dcooking_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
dcooking_c <- tbls_list_split(
  .data = dcooking_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) always or often added salt")

################################################################################



