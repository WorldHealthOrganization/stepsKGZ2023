################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dhealth <- function(.data) {
  
  # variable names that are used in the function
  dhealth_names <- c("sex", "d10")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dhealth_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dhealth_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(case_when(
          d10==1 ~ "1) think too much salt can cause health problems",
          d10==2 ~ "2) don't think too much salt can cause health problems",
          d10==77 ~ "3) don't know if salt can cause health problems",
          TRUE ~ NA_character_
        ),
        levels = c("1) think too much salt can cause health problems",
                   "2) don't think too much salt can cause health problems",
                   "3) don't know if salt can cause health problems")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
