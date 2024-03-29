################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Total physical activity per day (in mins)"

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

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Ptotal.R"))

ptotal_df <- ptotal(data_cleanrecodep1p15)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ptotal_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean minutes of total physical activity on average per day
ptotal_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = ptotalday)

# DATABOOK prep
ptotal_mn <- tbls_list_split(.data = ptotal_mn_list_long, .vars_amount_number = 0)

################################################################################

# Median minutes of total physical activity on average per day
ptotal_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = ptotalday)

# DATABOOK prep
ptotal_md <- tbls_list_split(.data = ptotal_md_list_long, .vars_amount_number = 0)

################################################################################




