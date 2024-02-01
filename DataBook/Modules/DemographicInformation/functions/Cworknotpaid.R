################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cworknotpaid <- function(.data) {
  
  # variable names used in function
  cworknotpaid_names <- c("sex", "c8")
  
  # check required columns
  if(!all(i <- rlang::has_name(.data, cworknotpaid_names))) {
    stop(sprintf(  
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cworknotpaid_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        c = factor(
          case_when(
            c8==4 ~ "1) Non-paid",
            c8==5 ~ "2) Student",
            c8==6 ~ "3) Homemaker",
            c8==7 ~ "4) Retired",
            c8==8 ~ "5) Unemployed able to work",
            c8==9 ~ "6) Unemployed unable to work",  
            TRUE ~ NA_character_
          ),
          levels = c("1) Non-paid", "2) Student", "3) Homemaker", "4) Retired",
                     "5) Unemployed able to work", "6) Unemployed unable to work")
        ),
        cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1)
      )
  }
}
