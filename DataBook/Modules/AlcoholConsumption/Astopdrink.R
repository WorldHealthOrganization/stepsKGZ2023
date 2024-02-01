################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Stopping drinking due to health reasons"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Astopdrink.R"))

astopdrink_df <- astopdrink(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- astopdrink_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Stopping drinking due to health reasons
astopdrink_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
astopdrink_c <- tbls_list_split(
  .data = astopdrink_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) stopped due to health reasons")

################################################################################


