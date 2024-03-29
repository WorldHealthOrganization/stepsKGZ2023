################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "Prevalence of smoking tobacco products among smokers"

################################################################################

# Load cleaned and weighted data

# Check if the datawt object exists in the global environment
if (!exists("datawt")) {
  source("LoadData.R", encoding="UTF-8")
}

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmoketypeprev.R"))

tsmoketypeprev_df <- tsmoketypeprev(datawt)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmoketypeprev_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Percentage of current smokers smoking each of the following products

# 1.1 manuf_cigs 
tsmoketypeprev_nondaily_t5a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5a <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5a_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) smokes manuf cigs")

# 1.2 handrolled_cigs
tsmoketypeprev_nondaily_t5b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5b <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5b_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) smokes hand-rolled cigs")

# 1.3 pipes 
tsmoketypeprev_nondaily_t5c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5c <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5c_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "1) smokes pipes")

# 1.4 cigars 
tsmoketypeprev_nondaily_t5d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5d <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5d_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "1) smokes cigars")

# 1.5 shisha 
tsmoketypeprev_nondaily_t5e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5e <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5e_list_long, 
  .select_var = g, .vars_amount_number = 2,
  .select_var_val = "1) smokes shisha")

# 1.6 heated_tobacco 
tsmoketypeprev_nondaily_t5f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = h, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5f <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5f_list_long, 
  .select_var = h, .vars_amount_number = 2,
  .select_var_val = "1) smokes heated tobacco products (iQOS, etc.)")

# 1.7 other_tobacco 
tsmoketypeprev_nondaily_t5g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = i, 
                                                      .cln2 = t1, .cln2_val = 1)
# DATABOOK prep
tsmoketypeprev_nondaily_t5g <- tbls_list_split(
  .data = tsmoketypeprev_nondaily_t5g_list_long, 
  .select_var = i, .vars_amount_number = 2,
  .select_var_val = "1) smokes other type of tobacco")

################################################################################

# Adding tobacco type column for joining the above into one single long list

if(.u_r_reg == FALSE) {
  
  tsmoketypeprev_nondaily_t5a_list_long_type <- 
    tsmoketypeprev_nondaily_t5a_list_long %>% mutate(., type = factor("Manufactured cigs")) 
  tsmoketypeprev_nondaily_t5b_list_long_type <- 
    tsmoketypeprev_nondaily_t5b_list_long %>% mutate(., type = factor("Hand-rolled cigs")) %>% rename(c=d)
  tsmoketypeprev_nondaily_t5c_list_long_type <- 
    tsmoketypeprev_nondaily_t5c_list_long %>% mutate(., type = factor("Pipes")) %>% rename(c=e)
  tsmoketypeprev_nondaily_t5d_list_long_type <- 
    tsmoketypeprev_nondaily_t5d_list_long %>% mutate(., type = factor("Cigars, cheerots, cigarillos")) %>% rename(c=f)
  tsmoketypeprev_nondaily_t5e_list_long_type <- 
    tsmoketypeprev_nondaily_t5e_list_long %>% mutate(., type = factor("Shisha")) %>% rename(c=g)
  tsmoketypeprev_nondaily_t5f_list_long_type <- 
    tsmoketypeprev_nondaily_t5f_list_long %>% mutate(., type = factor("Heated tobacco products")) %>% rename(c=h)
  tsmoketypeprev_nondaily_t5g_list_long_type <- 
    tsmoketypeprev_nondaily_t5g_list_long %>% mutate(., type = factor("Other type of tobacco")) %>% rename(c=i)
  
  # join all separate tables with full_join from dplyr
  nondaily_t5a_b <- full_join(tsmoketypeprev_nondaily_t5a_list_long_type, tsmoketypeprev_nondaily_t5b_list_long_type)
  nondaily_t5a_b_c <- full_join(nondaily_t5a_b, tsmoketypeprev_nondaily_t5c_list_long_type)
  nondaily_t5a_b_c_d <- full_join(nondaily_t5a_b_c, tsmoketypeprev_nondaily_t5d_list_long_type)
  nondaily_t5a_b_c_d_e <- full_join(nondaily_t5a_b_c_d, tsmoketypeprev_nondaily_t5e_list_long_type)
  nondaily_t5a_b_c_d_e_f <- full_join(nondaily_t5a_b_c_d_e, tsmoketypeprev_nondaily_t5f_list_long_type)
  nondaily_t5a_b_c_d_e_f_g <- full_join(nondaily_t5a_b_c_d_e_f, tsmoketypeprev_nondaily_t5g_list_long_type)
  tsmoketypeprev_nondaily_list_long <- nondaily_t5a_b_c_d_e_f_g
  
} else {
  # for cases when .u_r_reg == TRUE
  tsmoketypeprev_nondaily_t5a_list_long_type <- 
    tsmoketypeprev_nondaily_t5a_list_long %>% map(~ mutate(., type = factor("Manufactured cigs")))
  tsmoketypeprev_nondaily_t5b_list_long_type <- 
    tsmoketypeprev_nondaily_t5b_list_long %>% map(~ mutate(., type = factor("Hand-rolled cigs")))
  tsmoketypeprev_nondaily_t5c_list_long_type <- 
    tsmoketypeprev_nondaily_t5c_list_long %>% map(~ mutate(., type = factor("Pipes")))
  tsmoketypeprev_nondaily_t5d_list_long_type <- 
    tsmoketypeprev_nondaily_t5d_list_long %>% map(~ mutate(., type = factor("Cigars, cheerots, cigarillos")))
  tsmoketypeprev_nondaily_t5e_list_long_type <- 
    tsmoketypeprev_nondaily_t5e_list_long %>% map(~ mutate(., type = factor("Shisha")))
  tsmoketypeprev_nondaily_t5f_list_long_type <- 
    tsmoketypeprev_nondaily_t5f_list_long %>% map(~ mutate(., type = factor("Heated tobacco products")))
  tsmoketypeprev_nondaily_t5g_list_long_type <- 
    tsmoketypeprev_nondaily_t5g_list_long %>% map(~ mutate(., type = factor("Other type of tobacco")))
  
  # function to join lists
  f <- function(.x, .y) {
    if(is.list(.x)) purrr::map2(.x, .y, f) else c(.x, .y)
  }
  
  nondaily_t5a_b <- f(tsmoketypeprev_nondaily_t5a_list_long_type, tsmoketypeprev_nondaily_t5b_list_long_type)
  nondaily_t5a_b_c <- f(nondaily_t5a_b, tsmoketypeprev_nondaily_t5c_list_long_type)
  nondaily_t5a_b_c_d <- f(nondaily_t5a_b_c, tsmoketypeprev_nondaily_t5d_list_long_type)
  nondaily_t5a_b_c_d_e <- f(nondaily_t5a_b_c_d, tsmoketypeprev_nondaily_t5e_list_long_type)
  nondaily_t5a_b_c_d_e_f <- f(nondaily_t5a_b_c_d_e, tsmoketypeprev_nondaily_t5f_list_long_type)
  nondaily_t5a_b_c_d_e_f_g <- f(nondaily_t5a_b_c_d_e_f, tsmoketypeprev_nondaily_t5g_list_long_type) %>% map(~ as_tibble(.))
  tsmoketypeprev_nondaily_list_long <- nondaily_t5a_b_c_d_e_f_g
  
}

################################################################################

# 2 - Proportion of population who smoke each of the following products daily

# 2.1 manuf_cigs_daily
tsmoketypeprev_daily_t5a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = cd)
# DATABOOK prep
tsmoketypeprev_daily_t5a <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5a_list_long, 
  .select_var = cd, .vars_amount_number = 2,
  .select_var_val = "1) smokes manuf cigs daily")

# 2.2 handrolled_cigs_daily
tsmoketypeprev_daily_t5b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = dd)
# DATABOOK prep
tsmoketypeprev_daily_t5b <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5b_list_long, 
  .select_var = dd, .vars_amount_number = 2,
  .select_var_val = "1) smokes hand-rolled cigs daily")

# 2.3 pipes_daily
tsmoketypeprev_daily_t5c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = ed)
# DATABOOK prep
tsmoketypeprev_daily_t5c <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5c_list_long, 
  .select_var = ed, .vars_amount_number = 2,
  .select_var_val = "1) smokes pipes daily")

# 2.4 cigars_daily
tsmoketypeprev_daily_t5d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = fd)
# DATABOOK prep
tsmoketypeprev_daily_t5d <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5d_list_long, 
  .select_var = fd, .vars_amount_number = 2,
  .select_var_val = "1) smokes cigars daily")

# 2.5 shisha_daily
tsmoketypeprev_daily_t5e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = gd)
# DATABOOK prep
tsmoketypeprev_daily_t5e <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5e_list_long, 
  .select_var = gd, .vars_amount_number = 2,
  .select_var_val = "1) smokes shisha daily")

# 2.6 heated_tobacco_daily
tsmoketypeprev_daily_t5f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = hd)
# DATABOOK prep
tsmoketypeprev_daily_t5f <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5f_list_long, 
  .select_var = hd, .vars_amount_number = 2,
  .select_var_val = "1) smokes heated tobacco products (iQOS, etc.) daily")

# 2.7 other_tobacco_daily
tsmoketypeprev_daily_t5g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = id)
# DATABOOK prep
tsmoketypeprev_daily_t5g <- tbls_list_split(
  .data = tsmoketypeprev_daily_t5g_list_long, 
  .select_var = id, .vars_amount_number = 2,
  .select_var_val = "1) smokes other type of tobacco daily")

################################################################################

# Adding tobacco type column for joining the above into one single long list

if(.u_r_reg == FALSE) {
  tsmoketypeprev_daily_t5a_list_long_type <- 
    tsmoketypeprev_daily_t5a_list_long %>% mutate(., type = factor("Manufactured cigs"))
  tsmoketypeprev_daily_t5b_list_long_type <- 
    tsmoketypeprev_daily_t5b_list_long %>% mutate(., type = factor("Hand-rolled cigs")) %>% rename(cd=dd)
  tsmoketypeprev_daily_t5c_list_long_type <- 
    tsmoketypeprev_daily_t5c_list_long %>% mutate(., type = factor("Pipes")) %>% rename(cd=ed)
  tsmoketypeprev_daily_t5d_list_long_type <- 
    tsmoketypeprev_daily_t5d_list_long %>% mutate(., type = factor("Cigars, cheerots, cigarillos")) %>% rename(cd=fd)
  tsmoketypeprev_daily_t5e_list_long_type <- 
    tsmoketypeprev_daily_t5e_list_long %>% mutate(., type = factor("Shisha")) %>% rename(cd=gd)
  tsmoketypeprev_daily_t5f_list_long_type <- 
    tsmoketypeprev_daily_t5f_list_long %>% mutate(., type = factor("Heated tobacco products")) %>% rename(cd=hd)
  tsmoketypeprev_daily_t5g_list_long_type <- 
    tsmoketypeprev_daily_t5g_list_long %>% mutate(., type = factor("Other type of tobacco")) %>% rename(cd=id)
  
  # join all above tables with full_join from dplyr
  daily_t5a_b <- full_join(tsmoketypeprev_daily_t5a_list_long_type, tsmoketypeprev_daily_t5b_list_long_type)
  daily_t5a_b_c <- full_join(daily_t5a_b, tsmoketypeprev_daily_t5c_list_long_type)
  daily_t5a_b_c_d <- full_join(daily_t5a_b_c, tsmoketypeprev_daily_t5d_list_long_type)
  daily_t5a_b_c_d_e <- full_join(daily_t5a_b_c_d, tsmoketypeprev_daily_t5e_list_long_type)
  daily_t5a_b_c_d_e_f <- full_join(daily_t5a_b_c_d_e, tsmoketypeprev_daily_t5f_list_long_type)
  daily_t5a_b_c_d_e_f_g <- full_join(daily_t5a_b_c_d_e_f, tsmoketypeprev_daily_t5g_list_long_type)
  tsmoketypeprev_daily_list_long <- daily_t5a_b_c_d_e_f_g
  
} else {
  # for cases when .u_r_reg == TRUE
  tsmoketypeprev_daily_t5a_list_long_type <- 
    tsmoketypeprev_daily_t5a_list_long %>% map(~ mutate(., type = factor("Manufactured cigs")))
  tsmoketypeprev_daily_t5b_list_long_type <- 
    tsmoketypeprev_daily_t5b_list_long %>% map(~ mutate(., type = factor("Hand-rolled cigs")))
  tsmoketypeprev_daily_t5c_list_long_type <- 
    tsmoketypeprev_daily_t5c_list_long %>% map(~ mutate(., type = factor("Pipes")))
  tsmoketypeprev_daily_t5d_list_long_type <- 
    tsmoketypeprev_daily_t5d_list_long %>% map(~ mutate(., type = factor("Cigars, cheerots, cigarillos")))
  tsmoketypeprev_daily_t5e_list_long_type <- 
    tsmoketypeprev_daily_t5e_list_long %>% map(~ mutate(., type = factor("Shisha")))
  tsmoketypeprev_daily_t5f_list_long_type <- 
    tsmoketypeprev_daily_t5f_list_long %>% map(~ mutate(., type = factor("Heated tobacco products")))
  tsmoketypeprev_daily_t5g_list_long_type <- 
    tsmoketypeprev_daily_t5g_list_long %>% map(~ mutate(., type = factor("Other type of tobacco")))
  
  # function to join lists
  f <- function(.x, .y) {
    if(is.list(.x)) purrr::map2(.x, .y, f) else c(.x, .y)
  }
  
  daily_t5a_b <- f(tsmoketypeprev_daily_t5a_list_long_type, tsmoketypeprev_daily_t5b_list_long_type)
  daily_t5a_b_c <- f(daily_t5a_b, tsmoketypeprev_daily_t5c_list_long_type)
  daily_t5a_b_c_d <- f(daily_t5a_b_c, tsmoketypeprev_daily_t5d_list_long_type)
  daily_t5a_b_c_d_e <- f(daily_t5a_b_c_d, tsmoketypeprev_daily_t5e_list_long_type)
  daily_t5a_b_c_d_e_f <- f(daily_t5a_b_c_d_e, tsmoketypeprev_daily_t5f_list_long_type)
  daily_t5a_b_c_d_e_f_g <- f(daily_t5a_b_c_d_e_f, tsmoketypeprev_daily_t5g_list_long_type) %>% map(~ as_tibble(.))
  tsmoketypeprev_daily_list_long <- daily_t5a_b_c_d_e_f_g
}

################################################################################


