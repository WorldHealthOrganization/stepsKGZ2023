################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Levels of total physical activity"

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

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Ptotallevels.R"))

ptotallevels_df <- ptotallevels(data_cleanrecodep1p15)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ptotallevels_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Level of total physical activity according to former recommendations
ptotallevels_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
ptotallevels_c <- tbls_list_split(
  .data = ptotallevels_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################











