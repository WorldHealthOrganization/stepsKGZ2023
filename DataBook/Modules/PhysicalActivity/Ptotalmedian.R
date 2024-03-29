################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# "MEDIAN VALUES ONLY - Run Ptotal for mean values for total physical activity per day (in mins)"
# In median programs psu and stratum variables aren't used

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

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Ptotalmedian.R"))

ptotalmedian_df <- ptotalmedian(data_cleanrecodep1p15)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ptotalmedian_df %>% 
  as_survey_design(weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Median minutes of total physical activity on average per day
# Median with Interquartile range (P25-P75)
ptotalmedian_md_list_long <- 
  tbls_summary(.mn_pct_md = md, .variable = ptotalday)

# DATABOOK prep
ptotalmedian_md <- tbls_list_split(.data = ptotalmedian_md_list_long, .vars_amount_number = 0)

################################################################################

# FACTSHEET

# 18. Median time spent in physical activity on average per day (minutes) (presented with inter-quartile range)
fs_18_ptotalmedian_md_m <- fs_summary(filter(ptotalmedian_md$m, agerange == "18–69"), c(3,4,5), Males)
fs_18_ptotalmedian_md_w <- fs_summary(filter(ptotalmedian_md$w, agerange == "18–69"), c(3,4,5), Females)
fs_18_ptotalmedian_md_b <- fs_summary(filter(ptotalmedian_md$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_18_ptotalmedian_md_joint <- cbind(fs_18_ptotalmedian_md_b,
                                     fs_18_ptotalmedian_md_m,
                                     fs_18_ptotalmedian_md_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Median time spent in physical activity on average per day (minutes) (presented with inter-quartile range)", .before = 1)

readr::write_excel_csv(fs_18_ptotalmedian_md_joint, here("FactSheet", "18_fs_ptotalmedian_md.csv"))

################################################################################





