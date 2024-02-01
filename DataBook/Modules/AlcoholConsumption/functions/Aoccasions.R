################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

aoccasions <- function(.data) {
  
  aoccasions_names <- c("sex", "a1", "a2", "a5", "a6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aoccasions_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aoccasions_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(a1==1 & a2==1 & a5==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(a6) | a6==0 | a6>50, 2))
  }
  
}
