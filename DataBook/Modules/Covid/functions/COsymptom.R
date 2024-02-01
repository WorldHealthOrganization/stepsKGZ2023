################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cosymptom <- function(.data) {
  
  # variable names that are used in the function
  cosymptom_names <- c("sex", "co4", "co5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cosymptom_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cosymptom_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co5==1 | co5==2) & (co4==1 | co4==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(co5==1, "1) had symptoms", 
                          ifelse(co5==2, "2) had no symptoms", NA)),
                   levels = c("1) had symptoms", "2) had no symptoms"))
      )
  }
  
}
