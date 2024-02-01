################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mwaist <- function(.data) {
  
  # variable names that are used in the function
  mwaist_names <- c("sex", "m8", "m14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mwaist_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mwaist_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        m14cln = if_else(m14>=30 & m14<=200 & valid==1, 1, 2, missing = 2),
        m14cln = replace(m14cln, sex=="Women" & m8==1, 2)
      )
  }
  
}
