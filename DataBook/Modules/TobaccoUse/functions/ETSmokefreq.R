################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

etsmokefreq <- function(.data) {
  
  # variable names that are used in the function
  etsmokefreq_names <- c("sex", "et1", "et2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, etsmokefreq_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(etsmokefreq_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(et1==1 & (et2==1 | et2==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(et1==1 & et2==2, "2) current user (non-daily)", 
                          ifelse(et1==1 & et2==1, "1) current daily user", NA)),
                   levels = c("1) current daily user", "2) current user (non-daily)"))
      )
  }
  
}
