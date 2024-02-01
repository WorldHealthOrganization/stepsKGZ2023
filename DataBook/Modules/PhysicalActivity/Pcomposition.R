################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Percent of total physical activity per day per set" 

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

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Pcomposition.R"))

pcomposition_df <- pcomposition(data_cleanrecodep1p15)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- pcomposition_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# % Activity from work
pcomposition_work_list_long <- tbls_summary(.mn_pct_md = mn, .variable = percentwork)
# DATABOOK prep
pcomposition_work <- tbls_list_split(
  .data = pcomposition_work_list_long, .vars_amount_number = 0)


# % Activity for transport
pcomposition_trans_list_long <- tbls_summary(.mn_pct_md = mn, .variable = percenttrans)
# DATABOOK prep
pcomposition_trans <- tbls_list_split(
  .data = pcomposition_trans_list_long, .vars_amount_number = 0)


# % Activity during leisure time
pcomposition_rec_list_long <- tbls_summary(.mn_pct_md = mn, .variable = percentrec)
# DATABOOK prep
pcomposition_rec <- tbls_list_split(
  .data = pcomposition_rec_list_long, .vars_amount_number = 0)

################################################################################

# Adding activity type column for joining the above into one single long list

pcomposition_work_list_long_type <- pcomposition_work_list_long %>% map(~ mutate(., type = factor("Work")))
pcomposition_trans_list_long_type <- pcomposition_trans_list_long %>% map(~ mutate(., type = factor("Transport")))
pcomposition_rec_list_long_type <- pcomposition_rec_list_long %>% map(~ mutate(., type = factor("Leisure")))

f <- function(.x, .y) {
  if(is.list(.x)) purrr::map2(.x, .y, f) else c(.x, .y)
}

work_trans <- f(pcomposition_work_list_long_type, pcomposition_trans_list_long_type)
work_trans_rec <- f(work_trans, pcomposition_rec_list_long_type) %>% map(~ as_tibble(.))
pcomposition_list_long <- work_trans_rec

################################################################################


