################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coresolved <- function(.data) {
  
  # variable names that are used in the function
  coresolved_names <- c("sex", "co5", "co8a_type", "co9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coresolved_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coresolved_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co9==1 | co9==2) & co5==1 & co8a_type!=5 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(co9==1, "1) resolved", 
                          ifelse(co9==2, "2) unresolved", NA)),
                   levels = c("1) resolved", "2) unresolved"))
      )
  }
  
}

