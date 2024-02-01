################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Long COVID symptoms"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "Covid", "functions", "COsymptom.R"))

cosymptom_df <- cosymptom(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cosymptom_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents out of general population who had symptoms which lasted 
# longer than 2 months after the acute infection, which may imply that they may 
# have "Long COVID" or "Post-COVID condition"
cosymptom_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
cosymptom_c <- tbls_list_split(
  .data = cosymptom_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) had symptoms")

################################################################################




