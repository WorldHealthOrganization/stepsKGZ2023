################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cmaritalstatus <- function(.data) {
  
  # variable names used in function
  cmaritalstatus_names <- c("sex", "c7")
  
  # check required columns 
  if(!all(i <- rlang::has_name(.data, cmaritalstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cmaritalstatus_names[!i], collapse=", ")
    ))
  } else {
    .data %>%  
      mutate(
        c = factor(
          case_when(
            c7==1 ~ "1) Never Married",
            c7==2 ~ "2) Currently married",
            c7==3 ~ "3) Separated",
            c7==4 ~ "4) Divorced",
            c7==5 ~ "5) Widowed",
            c7==6 ~ "6) Cohabitating",
            TRUE ~ NA_character_
          ),
          levels = c("1) Never Married", "2) Currently married", "3) Separated",
                     "4) Divorced", "5) Widowed", "6) Cohabitating")
        ),
        cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1)
      )
  }
}
