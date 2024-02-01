################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

ddays <- function(.data) {
  
  # variable names that are used in the function
  ddays_names <- c("sex", "d1", "d3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ddays_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ddays_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        d1cln = if_else(is.na(d1) | d1>7 | valid==2, 2, 1, missing = 1),
        d3cln = if_else(is.na(d3) | d3>7 | valid==2, 2, 1, missing = 1)
      )
  }
  
}
