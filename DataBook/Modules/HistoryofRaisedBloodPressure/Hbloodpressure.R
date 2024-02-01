################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Blood pressure measurement, diagnosis and medication"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedBloodPressure", "functions", "Hbloodpressure.R"))

hbloodpressure_df <- hbloodpressure(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hbloodpressure_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Blood pressure measurement and diagnosis
hbloodpressure_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
hbloodpressure_c <- tbls_list_split(
  .data = hbloodpressure_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking drugs (medication) for raised blood pressure 
# prescribed by doctor or health worker among those diagnosed
hbloodpressure_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h3cln)

# DATABOOK prep
hbloodpressure_d <- tbls_list_split(
  .data = hbloodpressure_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################
################################################################################

# Ever told by a health care worker about having raised blood pressure or hypertension

# TODO:
# proposed code (using separate CLN variable for h2a, excluding h1==2, h2a==2 and NAs):
# hbloodpressure_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = h2acln2)

# format used in UKR (same CLN variable):
hbloodpressure_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e)

# DATABOOK prep
hbloodpressure_e <- tbls_list_split(
  .data = hbloodpressure_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) was told")
