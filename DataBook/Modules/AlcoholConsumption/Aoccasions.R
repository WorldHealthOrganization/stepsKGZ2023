################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Drinking occasions in the past 30 days"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aoccasions.R"))

aoccasions_df <- aoccasions(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aoccasions_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of drinking occasions in the past 30 days among current (past 30 days) drinkers
aoccasions_a6_list_long <- tbls_summary(.mn_pct_md = mn, .variable = a6)

# DATABOOK prep
aoccasions_a6 <- tbls_list_split(.data = aoccasions_a6_list_long, .vars_amount_number = 0)

################################################################################


