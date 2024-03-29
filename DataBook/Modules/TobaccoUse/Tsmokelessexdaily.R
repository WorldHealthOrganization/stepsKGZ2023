################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Percentage of ex-daily smokeless users in the population"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokelessexdaily.R"))

tsmokelessexdaily_df <- tsmokelessexdaily(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokelessexdaily_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Former daily smokeless tobacco users (who don't use tobacco currently) among all respondents
tsmokelessexdaily_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
tsmokelessexdaily_c <- tbls_list_split(
  .data = tsmokelessexdaily_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "Ex-daily user")

################################################################################

# Former daily smokeless tobacco users (who don’t use tobacco currently) among ever daily users
tsmokelessexdaily_everdaily_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, 
                                                        .cln2 = everdaily, .cln2_val = 1)

# DATABOOK prep
tsmokelessexdaily_everdaily_c <- tbls_list_split(
  .data = tsmokelessexdaily_everdaily_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "Ex-daily user")

################################################################################


