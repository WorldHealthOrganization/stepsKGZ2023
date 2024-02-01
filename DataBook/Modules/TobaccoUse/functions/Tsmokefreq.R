################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokefreq <- function(.data) {
  
  # variable names that are used in the function
  tsmokefreq_names <- c("sex", "t1", "t2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokefreq_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokefreq_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(t1==1 & (t2==1 | t2==2) & valid==1, 1, 2, missing = 2), 
        c = factor(ifelse(t1==1 & t2==2, "current smoker (non-daily)",  
                          ifelse(t1==1 & t2==1, "current daily smoker", NA)),
                   levels = c("current smoker (non-daily)", "current daily smoker"))
      )
  }
  
}
