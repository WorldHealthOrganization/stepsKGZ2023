################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hcvd <- function(.data) {
  
  # variable names that are used in the function
  hcvd_names <- c("sex", "h17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcvd_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcvd_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        h17cln = if_else((h17==1 | h17==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(h17==1, "1) had heart attack/chest pain",
                          ifelse(h17==2, "2) did not have heart attack/chest pain", NA)),
                   levels = c("1) had heart attack/chest pain",
                              "2) did not have heart attack/chest pain"))
      )
  }
  
}
