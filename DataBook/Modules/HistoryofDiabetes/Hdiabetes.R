################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Diabetes measurement, diagnosis and treatment"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofDiabetes", "functions", "Hdiabetes.R"))

hdiabetes_df <- hdiabetes(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hdiabetes_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Blood sugar measurement and diagnosis
hdiabetes_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
hdiabetes_c <- tbls_list_split(
  .data = hdiabetes_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking drugs (medication) prescribed for diabetes among those previously diagnosed
hdiabetes_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h8cln)

# DATABOOK prep
hdiabetes_d <- tbls_list_split(
  .data = hdiabetes_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################

# Currently taking insulin prescribed for diabetes among those previously diagnosed
hdiabetes_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = h9cln)

# DATABOOK prep
hdiabetes_e <- tbls_list_split(
  .data = hdiabetes_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) taking insulin")

################################################################################


# Ever told by a health care worker about having raised blood glucose or diabetes

# TODO:
# proposed code (using separate CLN variable for h7a, excluding h6==2, h7a==2 and NAs):
# hdiabetes_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = h7acln2)

# format used in UKR (same CLN variable):
hdiabetes_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f)

# DATABOOK prep
hdiabetes_f <- tbls_list_split(
  .data = hdiabetes_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "1) was told")

################################################################################

# Aspirin intake among people with diagnosed / history of diabetes.
hdiabetes_g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, .cln = h18cln1)

# DATABOOK prep
hdiabetes_g <- tbls_list_split(
  .data = hdiabetes_g_list_long, .select_var = g, 
  .vars_amount_number = 2, .select_var_val = "1) taking aspirin")

################################################################################

# Statins intake among people with diagnosed / history of diabetes.
hdiabetes_h_list_long <- tbls_summary(.mn_pct_md = pct, .variable = h, .cln = h19cln1)

# DATABOOK prep
hdiabetes_h <- tbls_list_split(
  .data = hdiabetes_h_list_long, .select_var = h, 
  .vars_amount_number = 2, .select_var_val = "1) taking statins")

################################################################################

# Aspirin intake among people with diagnosed / history of diabetes or blood glucose>=7 mmol/l or took meds today.
hdiabetes_i_list_long <- tbls_summary(.mn_pct_md = pct, .variable = i, .cln = h18cln2)

# DATABOOK prep
hdiabetes_i <- tbls_list_split(
  .data = hdiabetes_i_list_long, .select_var = i, 
  .vars_amount_number = 2, .select_var_val = "1) taking aspirin")

################################################################################

# Statins intake among people with diagnosed / history of diabetes or blood glucose>=7 mmol/l or took meds today.
hdiabetes_j_list_long <- tbls_summary(.mn_pct_md = pct, .variable = j, .cln = h19cln2)

# DATABOOK prep
hdiabetes_j <- tbls_list_split(
  .data = hdiabetes_j_list_long, .select_var = j, 
  .vars_amount_number = 2, .select_var_val = "1) taking statins")


