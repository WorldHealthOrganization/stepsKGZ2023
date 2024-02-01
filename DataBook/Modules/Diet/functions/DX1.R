################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dx1func <- function(.data) {
  
  # variable names that are used in the function
  dx1_names <- c("sex", "dx1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dx1_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dx1_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(case_when(
          dx1==1 ~ "1) Once or more a day",
          dx1==2 ~ "2) 4 to 6 times a week",
          dx1==3 ~ "3) 1 to 3 times a week",
          dx1==4 ~ "4) Less than once a week",
          dx1==5 ~ "5) Never",
          TRUE ~ NA_character_
        ), levels = c(
          "1) Once or more a day",
          "2) 4 to 6 times a week",
          "3) 1 to 3 times a week",
          "4) Less than once a week",
          "5) Never"
        )),
        cln = if_else((dx1==1 | dx1==2 | dx1==3 | dx1==4 | dx1==5) & valid==1, 1, 2, missing = 2)
      )
  }
  
}
