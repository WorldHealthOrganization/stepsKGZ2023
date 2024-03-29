################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Heart attack/chest pain from heart disease or stroke"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofCardiovascularDiseases", "functions", "Hcvd.R"))

hcvd_df <- hcvd(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hcvd_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Having ever had a heart attack or chest pain from heart disease or a stroke
hcvd_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h17cln)

# DATABOOK prep
hcvd_c <- tbls_list_split(
  .data = hcvd_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) had heart attack/chest pain")

################################################################################




