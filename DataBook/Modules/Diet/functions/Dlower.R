################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dlower <- function(.data) {
  
  # variable names that are used in the function
  dlower_names <- c("sex", "d9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dlower_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dlower_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(case_when(
          d9==1 ~ "1) very important",
          d9==2 ~ "2) somewhat important",
          d9==3 ~ "3) not at all important",
          TRUE ~ NA_character_
        ),
        levels = c("1) very important", "2) somewhat important", "3) not at all important")),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
