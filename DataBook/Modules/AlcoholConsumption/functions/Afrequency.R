################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

afrequency <- function(.data) {
  
  # variable names that are used in the function
  afrequency_names <- c("sex", "a1", "a2", "a4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afrequency_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afrequency_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(case_when(
          a4==1 ~ "1) Daily",
          a4==2 ~ "2) 5-6 days per week",
          a4==3 ~ "3) 3-4 days per week",
          a4==4 ~ "4) 1-2 days per week",
          a4==5 ~ "5) 1-3 days per month",
          a4==6 ~ "6) less than once a month",
          a4==7 ~ "7) Never",
          TRUE ~ NA_character_
        ), levels = c(
          "1) Daily", "2) 5-6 days per week", "3) 3-4 days per week", "4) 1-2 days per week", 
          "5) 1-3 days per month", "6) less than once a month", "7) Never"
        )),
        cln = if_else(a1==1 & a2==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
    
  }
  
}
