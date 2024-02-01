################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coconfirm <- function(.data) {
  
  # variable names that are used in the function
  coconfirm_names <- c("sex", "co4", "co4a")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coconfirm_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coconfirm_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co4a==1 | co4a==2 | co4a==3 | co4a==4) & (co4==1 | co4==2) & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          co4a==1 ~ "1) Yes, tested positive",
          co4a==2 ~ "2) No, tested negative",
          co4a==3 ~ "3) No, didn't want to get tested",
          co4a==4 ~ "4) No, test was unavailable",
          TRUE ~ NA_character_
        ),
        levels = c("1) Yes, tested positive", "2) No, tested negative", 
                   "3) No, didn't want to get tested", "4) No, test was unavailable"))
      )
  }
  
}

