################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

aconsumption <- function(.data) {
  
  aconsumption_names <- c("sex", "a1", "a2", "a5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aconsumption_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aconsumption_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2),
        a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2),
        a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1),
        a5cln = if_else(a2==1 & (a5==1 | a5==2), 1, 2, missing = 2),
        a5cln = replace(a5cln, a2==2 & (is.na(a5) | a5==2), 1),
        a5cln = replace(a5cln, a1==2 & (is.na(a5) | a5==2), 1),
        cln = if_else(a1cln==1 & a2cln==1 & a5cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          a5==1 ~ "1) drank in past 30 days (current drinker)",
          a2==1 & a5==2 ~ "2) drank in last 12 months, but not current drinker",
          a1==1 & a2==2 ~ "3) past 12 mos. abstainer",
          a1==2 ~ "4) lifetime abstainer",
          TRUE ~ NA_character_
        ), levels = c(
          "1) drank in past 30 days (current drinker)",
          "2) drank in last 12 months, but not current drinker",
          "3) past 12 mos. abstainer",
          "4) lifetime abstainer"
        ))
      )
  }
  
}
