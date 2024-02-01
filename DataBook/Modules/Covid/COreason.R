################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Reasons against vaccination from COVID-19"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Covid", "functions", "COreason.R"))

coreason_df <- coreason(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- coreason_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - CO3a. Inconvenient location of the health facility that offers the vaccine
coreason_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = co3acln)
# DATABOOK prep
coreason_a <- tbls_list_split(
  .data = coreason_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "1) Inconvenient location")


# 2 - CO3b. Preferred vaccine option is unavailable
coreason_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = co3bcln)
# DATABOOK prep
coreason_b <- tbls_list_split(
  .data = coreason_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "1) Preferred vaccine unavailable")


# 3 - CO3c. Has a medical condition that makes the vaccine unsafe for the participant
coreason_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = co3ccln)
# DATABOOK prep
coreason_c <- tbls_list_split(
  .data = coreason_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) Has a medical condition")


# 4 - CO3d. Health worker has not recommended the COVID-19 vaccine
coreason_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = co3dcln)
# DATABOOK prep
coreason_d <- tbls_list_split(
  .data = coreason_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) Was not recommended")


# 5 - CO3e. Has concerns about a severe reaction to the vaccine
coreason_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = co3ecln)
# DATABOOK prep
coreason_e <- tbls_list_split(
  .data = coreason_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) Has concerns about adverse reactions")


# 6 - CO3f. Does not think the vaccine is safe in general
coreason_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = co3fcln)
# DATABOOK prep
coreason_f <- tbls_list_split(
  .data = coreason_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "1) Thinks vaccine is unsafe")


# 7 - CO3g. Does not think the vaccine will protect the participant against severe disease from COVID-19 infection 
coreason_g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, .cln = co3gcln)
# DATABOOK prep
coreason_g <- tbls_list_split(
  .data = coreason_g_list_long, .select_var = g, 
  .vars_amount_number = 2, .select_var_val = "1) Thinks vaccine won't protect")


# 8 - CO3h. Believes there might be long-term side effects of the vaccine
coreason_h_list_long <- tbls_summary(.mn_pct_md = pct, .variable = h, .cln = co3hcln)
# DATABOOK prep
coreason_h <- tbls_list_split(
  .data = coreason_h_list_long, .select_var = h, 
  .vars_amount_number = 2, .select_var_val = "1) Believes in long-term side effects")


# 9 - CO3i. Does not trust the government/medical authorities
coreason_i_list_long <- tbls_summary(.mn_pct_md = pct, .variable = i, .cln = co3icln)
# DATABOOK prep
coreason_i <- tbls_list_split(
  .data = coreason_i_list_long, .select_var = i, 
  .vars_amount_number = 2, .select_var_val = "1) No trust in government/medical authorities")


# 10 - CO3j. Has no concerns about getting infected with COVID-19
coreason_j_list_long <- tbls_summary(.mn_pct_md = pct, .variable = j, .cln = co3jcln)
# DATABOOK prep
coreason_j <- tbls_list_split(
  .data = coreason_j_list_long, .select_var = j, 
  .vars_amount_number = 2, .select_var_val = "1) No concerns about getting infected")


# 11 - CO3k. Believes that natural immunity is better than getting vaccinated
coreason_k_list_long <- tbls_summary(.mn_pct_md = pct, .variable = k, .cln = co3kcln)
# DATABOOK prep
coreason_k <- tbls_list_split(
  .data = coreason_k_list_long, .select_var = k, 
  .vars_amount_number = 2, .select_var_val = "1) Believes in natural immunity")


# 12 - CO3l. Other reason
coreason_l_list_long <- tbls_summary(.mn_pct_md = pct, .variable = l, .cln = co3lcln)
# DATABOOK prep
coreason_l <- tbls_list_split(
  .data = coreason_l_list_long, .select_var = l, 
  .vars_amount_number = 2, .select_var_val = "1) Other reason")



################################################################################


