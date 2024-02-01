################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpcigads <- function(.data) {
  
  # variable names that are used in the function
  tpcigads_names <- c("sex", "tp2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpcigads_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpcigads_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(tp2==1, "1) noticed information", 
                          ifelse(tp2==2, "2) did not notice information", NA)),
                   levels = c("1) noticed information", "2) did not notice information")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      ) 
  }
  
}
