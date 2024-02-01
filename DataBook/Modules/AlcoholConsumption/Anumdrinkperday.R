################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Number of standard drinks per drinking day"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Anumdrinkperday.R"))

anumdrinkperday_df <- anumdrinkperday(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- anumdrinkperday_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of standard drinks per drinking occasion among current (past 30 days) drinkers
anumdrinkperday_a7_list_long <- tbls_summary(.mn_pct_md = mn, .variable = a7)

# DATABOOK prep
anumdrinkperday_a7 <- tbls_list_split(.data = anumdrinkperday_a7_list_long, .vars_amount_number = 0)

################################################################################


