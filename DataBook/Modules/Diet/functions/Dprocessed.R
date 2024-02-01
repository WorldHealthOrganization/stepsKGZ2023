################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dprocessed <- function(.data) {
  
  # variable names that are used in the function
  dprocessed_names <- c("sex", "d7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dprocessed_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dprocessed_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(d7==1 | d7==2, "1) always or often consume salty processed food", 
                          ifelse(d7==3 | d7==4 | d7==5, "2) sometimes/rarely/never consume salty processed food", NA)),
                   levels = c("1) always or often consume salty processed food",
                              "2) sometimes/rarely/never consume salty processed food")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
