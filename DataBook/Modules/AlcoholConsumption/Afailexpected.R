################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Frequency of failing to do what was expected"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Afailexpected.R"))

afailexpected_df <- afailexpected(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- afailexpected_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of failing to do what was normally expected from you during 
# the past 12 months among past 12 month drinkers
afailexpected_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
afailexpected_c <- 
  tbls_list_split(.data = afailexpected_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################

