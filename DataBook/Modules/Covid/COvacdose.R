################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "The number of vaccine doses received"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COvacdose.R"))

covacdose_df <- covacdose(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- covacdose_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Number of vaccine doses received out of those who are vaccinated
covacdose_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
covacdose_c <- tbls_list_split(
  .data = covacdose_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################

