################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Time off work or studying due to long COVID symptoms"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COtimeoff.R"))

cotimeoff_df <- cotimeoff(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cotimeoff_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of population that required time off work or studying due to long COVID symptoms
cotimeoff_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
cotimeoff_c <- tbls_list_split(
  .data = cotimeoff_c_list_long, .select_var = c, .vars_amount_number = 3)
# m1=Yes
# m2=No
# m3=Don’t work or study
################################################################################

# Mean duration in days required for time off work or studying
cotimeoff_co8adays_list_long <- 
  tbls_summary(.mn_pct_md = mn, .variable = co8adays, .cln2 = co8acln, .cln2_val = 1)

# DATABOOK prep
cotimeoff_co8adays <- 
  tbls_list_split(.data = cotimeoff_co8adays_list_long, .vars_amount_number = 0)




