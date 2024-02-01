################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

covac <- function(.data) {
  
  # variable names that are used in the function
  covac_names <- c("sex", "co1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, covac_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(covac_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co1==1 | co1==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(co1==1, "1) has been vaccinated", 
                          ifelse(co1==2, "2) has not been vaccinated", NA)),
                   levels = c("1) has been vaccinated", "2) has not been vaccinated"))
        )
  }
  
}
