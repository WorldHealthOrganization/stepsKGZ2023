################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

anumdrinkperday <- function(.data) {
  
  anumdrinkperday_names <- c("sex", "a1", "a2", "a5", "a7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, anumdrinkperday_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(anumdrinkperday_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(a1==1 & a2==1 & a5==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(a7) | a7==0 | a7>50, 2))
  }
  
}
