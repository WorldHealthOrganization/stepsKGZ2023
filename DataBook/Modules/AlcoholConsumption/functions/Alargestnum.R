################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

alargestnum <- function(.data) {
  
  # variable names that are used in the function
  alargestnum_names <- c("sex", "a1", "a2", "a5", "a8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, alargestnum_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(alargestnum_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(a1==1 & a2==1 & a5==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(a8) | a8==0 | a8>50, 2)
      )
  }
  
}
