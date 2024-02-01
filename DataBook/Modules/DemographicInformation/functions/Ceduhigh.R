################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

ceduhigh <- function(.data) {
  
  # variable names used in the function
  ceduhigh_names <- c("sex", "c5")
  
  # check required columns before proceeding
  if(!all(i <- rlang::has_name(.data, ceduhigh_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ceduhigh_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        c = factor(
          case_when(
            c5==1 ~ "1) No formal schooling",
            c5==2 ~ "2) Primary school",
            c5==3 ~ "3) Basic (not completed) secondary school (8-9 grade)",
            c5==4 ~ "4) Basic (completed) secondary school (10-11 grade)",
            c5==5 ~ "5) Initial vocational education (vocational school, liceum)",  
            c5==6 ~ "6) Secondary vocational education (technical college, other college)",
            c5==7 ~ "7) Higher education",
            TRUE ~ NA_character_
          ),
          levels = c("1) No formal schooling", "2) Primary school", 
                     "3) Basic (not completed) secondary school (8-9 grade)",
                     "4) Basic (completed) secondary school (10-11 grade)",
                     "5) Initial vocational education (vocational school, liceum)",
                     "6) Secondary vocational education (technical college, other college)",
                     "7) Higher education")
        )
      ) %>%
      mutate(
        cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1)
      )
  }
}
