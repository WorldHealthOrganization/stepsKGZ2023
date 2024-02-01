################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coinfect <- function(.data) {
  
  # variable names that are used in the function
  coinfect_names <- c("sex", "co4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coinfect_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coinfect_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(co4==1 | co4==2 | co4==3 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          co4==1 ~ "1) Yes, had it once",
          co4==2 ~ "2) Yes, had it multiple times",
          co4==3 ~ "3) No",
          TRUE ~ NA_character_
        ),
        levels = c("1) Yes, had it once", "2) Yes, had it multiple times", "3) No"))
      )
  }
  
}

