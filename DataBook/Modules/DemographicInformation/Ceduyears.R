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

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Ceduyears.R"))

ceduyears_df <- ceduyears(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ceduyears_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of years of education
ceduyears_list_long <- tbls_summary(.wt_unwt = unwt, .mn_pct_md = mn, .variable = c4)

# DATABOOK prep
ceduyears <- 
  tbls_list_split(.data = ceduyears_list_long, .vars_amount_number = 0)

################################################################################

