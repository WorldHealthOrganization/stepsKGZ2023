################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Cholesterol measurement, diagnosis and medication"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedTotalCholesterol", "functions", "Hchol.R"))

hchol_df <- hchol(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hchol_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Total cholesterol measurement and diagnosis
hchol_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
hchol_c <- tbls_list_split(
  .data = hchol_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking oral treatment (medication) prescribed for 
# raised total cholesterol among those previously diagnosed
hchol_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h14cln)

# DATABOOK prep
hchol_d <- tbls_list_split(
  .data = hchol_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################
################################################################################

# Ever told by a health care worker about having raised cholesterol

# TODO:
# proposed code (using separate CLN variable for h13a, excluding h12==2, h13a==2 and NAs):
# hchol_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = h13acln2)

# format used in UKR (same CLN variable):
hchol_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e)

# DATABOOK prep
hchol_e <- tbls_list_split(
  .data = hchol_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) was told")


