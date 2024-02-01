################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# NOTE: THIS SCRIPT COVERS TWO PROGRAMS (MEANS & MEDIANS)

# "Sedentary Time on a Typical Day" 
# In median programs psu and stratum variables aren't used

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load and apply the Clean Recode P16 function to datawt

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP16.R"))

data_cleanrecodep16 <- cleanrecodep16(datawt)

################################################################################

# Load the function for this indicator and apply to data_cleanrecodep16

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Psedentary.R"))

psedentary_df <- psedentary(data_cleanrecodep16)

################################################################################

library(srvyr)

# Specifying design for MEANS

STEPSClean <- psedentary_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# NOTE HERE THE USE OF FUNCTIONS SPECIFIC FOR PHYSICAL ACTIVITY

# Minutes spent in sedentary activities on average per day
# MEAN MINUTES
psedentary_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = p16)
# DATABOOK prep
psedentary_mn <- tbls_list_split(
  .data = psedentary_mn_list_long, .vars_amount_number = 0)

# MEDIAN MINUTES
psedentary_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = p16)
# DATABOOK prep
psedentary_md <- tbls_list_split(
  .data = psedentary_md_list_long, .vars_amount_number = 0)

################################################################################

