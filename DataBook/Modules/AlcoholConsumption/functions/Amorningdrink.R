################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

amorningdrink <- function(.data) {
  
  amorningdrink_names <- c("sex", "a1", "a2", "a15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, amorningdrink_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(amorningdrink_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        c = factor(ifelse(a15==1 | a15==2 | a15==3, "1) monthly or more frequently",
                          ifelse(a15==4, "2) less than monthly",
                                 ifelse(a15==5, "3) never", NA))),
                   levels=c("1) monthly or more frequently", 
                            "2) less than monthly",
                            "3) never")),
        cln = if_else(a1==1 & a2==1 & a15>=1 & a15<=5 & valid==1, 1, 2, missing = 2)
      )
  }
  
}
