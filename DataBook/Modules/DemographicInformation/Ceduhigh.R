################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Education"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Ceduhigh.R"))

ceduhigh_df <- ceduhigh(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ceduhigh_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Highest level of education
ceduhigh_c_list_long <- tbls_summary(.wt_unwt = unwt, .mn_pct_md = pct, .variable = c)

ceduhigh_c <- tbls_list_split(.data = ceduhigh_c_list_long, .select_var = c, .vars_amount_number = 7)

################################################################################



