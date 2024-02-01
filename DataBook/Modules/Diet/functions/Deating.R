################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

deating <- function(.data) {
  
  # variable names that are used in the function
  deating_names <- c("sex", "d5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, deating_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(deating_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(d5==1 | d5==2, "1) always or often added salt", 
                          ifelse(d5==3 | d5==4 | d5==5, "2) sometimes/rarely/never added salt", NA)),
                   levels = c("1) always or often added salt", 
                              "2) sometimes/rarely/never added salt")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
