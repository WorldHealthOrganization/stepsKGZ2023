################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coability <- function(.data) {
  
  # variable names that are used in the function
  coability_names <- c("sex", "co5", "co7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coability_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coability_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co7==1 | co7==2 | co7==3) & co5==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          co7==1 ~ "1) Yes, a lot",
          co7==2 ~ "2) Yes, a little",
          co7==3 ~ "3) Not at all",
          TRUE ~ NA_character_
        ),
        levels = c("1) Yes, a lot", "2) Yes, a little", "3) Not at all"))
      )
  }
  
}

