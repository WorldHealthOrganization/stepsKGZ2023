################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tetswork <- function(.data) {
  
  # variable names that are used in the function
  tetswork_names <- c("sex", "t18")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tetswork_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tetswork_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((t18==1 | t18==2) & valid==1, 1, 2, missing = 2),
        c = factor(if_else(t18==1, "exposed at work", 
                           if_else(t18==2, "not exposed", NA)),
                   levels = c("exposed at work", "not exposed"))
      )
  }
}
