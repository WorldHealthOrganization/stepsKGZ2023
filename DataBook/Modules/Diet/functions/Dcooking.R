################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dcooking <- function(.data) {
  
  # variable names that are used in the function
  dcooking_names <- c("sex", "d6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dcooking_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dcooking_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(d6==1 | d6==2, "1) always or often added salt", 
                          ifelse(d6==3 | d6==4 | d6==5, "2) sometimes/rarely/never added salt", NA)),
                   levels = c("1) always or often added salt", 
                              "2) sometimes/rarely/never added salt")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
