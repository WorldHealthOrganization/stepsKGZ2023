################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "No physical activity by setting"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load and apply the Clean Recode P1-P15 function to datawt

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data_cleanrecodep1p15 <- cleanrecodep1p15(datawt)

################################################################################

# Load the function for this indicator and apply to data_cleanrecodep1p15

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Pnoactivitybyset.R"))

pnoactivitybyset_df <- pnoactivitybyset(data_cleanrecodep1p15)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- pnoactivitybyset_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - No work-related physical activity
pnoactivitybyset_work_list_long <- tbls_summary(.mn_pct_md = pct, .variable = work)

# DATABOOK prep
pnoactivitybyset_work <- tbls_list_split(
  .data = pnoactivitybyset_work_list_long, 
  .select_var = work, .vars_amount_number = 2,
  .select_var_val = "did no work activity")


# 2 - No transport-related physical activity
pnoactivitybyset_trans_list_long <- tbls_summary(.mn_pct_md = pct, .variable = trans)

# DATABOOK prep
pnoactivitybyset_trans <- tbls_list_split(
  .data = pnoactivitybyset_trans_list_long, 
  .select_var = trans, .vars_amount_number = 2,
  .select_var_val = "did no transport activity")


# 3 - No recreation-related physical activity
pnoactivitybyset_rec_list_long <- tbls_summary(.mn_pct_md = pct, .variable = rec)

# DATABOOK prep
pnoactivitybyset_rec <- tbls_list_split(
  .data = pnoactivitybyset_rec_list_long, 
  .select_var = rec, .vars_amount_number = 2,
  .select_var_val = "did no recreation activity")


################################################################################






