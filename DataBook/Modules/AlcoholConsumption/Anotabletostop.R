################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Frequency of being unable to stop drinking"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Anotabletostop.R"))

anotabletostop_df <- anotabletostop(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- anotabletostop_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of not being able to stop drinking once started 
# during the past 12 months among past 12 month drinkers
anotabletostop_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
anotabletostop_c <- 
  tbls_list_split(.data = anotabletostop_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################


