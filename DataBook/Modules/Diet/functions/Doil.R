################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

doil <- function(.data) {
  
  # variable names that are used in the function
  doil_names <- c("sex", "d12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, doil_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(doil_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        d12cln = if_else(is.na(d12) | d12>7, 2, 1, missing = 1),
        cln = if_else(d12cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          d12==1 ~ "1) Vegetable oil",
          d12==2 ~ "2) Lard or Suet",
          d12==3 ~ "3) Butter or Ghee",
          d12==4 ~ "4) Margarine",
          d12==5 ~ "5) Other",
          d12==6 ~ "6) None in particular",
          d12==7 ~ "7) None used",
          TRUE ~ NA_character_
        ),
        levels = c("1) Vegetable oil", "2) Lard or Suet", "3) Butter or Ghee", "4) Margarine", 
                   "5) Other", "6) None in particular", "7) None used"))
      )
  }
  
}
