################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Medication use to prevent/treat heart disease"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofCardiovascularDiseases", "functions", "Hcvdmeds.R"))

hcvdmeds_df <- hcvdmeds(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hcvdmeds_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# General population taking aspirin or statin

# 1 - Currently taking aspirin regularly to prevent or treat heart disease
hcvdmeds_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h18cln)
# DATABOOK prep
hcvdmeds_c <- tbls_list_split(
  .data = hcvdmeds_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) taking aspirin")

# 2 - Currently taking statins regularly to prevent or treat heart disease
hcvdmeds_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h19cln)
# DATABOOK prep
hcvdmeds_d <- tbls_list_split(
  .data = hcvdmeds_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking statins")

################################################################################

# Subset of people with CVD taking aspirin or statins
# 3 - Respondents with CVD currently taking aspirin 
hcvdmeds_h17_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h18cln1)
# DATABOOK prep
hcvdmeds_h17_c <- tbls_list_split(
  .data = hcvdmeds_h17_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) taking aspirin")

# 4 - Respondents with CVD currently taking statins
hcvdmeds_h17_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h19cln1)
# DATABOOK prep
hcvdmeds_h17_d <- tbls_list_split(
  .data = hcvdmeds_h17_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking statins")

################################################################################
################################################################################



