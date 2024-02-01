################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

acategories <- function(.data) {
  
  # variable names that are used in the function
  acategories_names <- c("sex", "a1", "a2", "a5", "a7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, acategories_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(acategories_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2),
        a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2),
        a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1),
        a5cln = if_else(a2==1 & (a5==1 | a5==2), 1, 2, missing = 2),
        a5cln = replace(a5cln, a2==2 & (is.na(a5) | a5==2), 1),
        a5cln = replace(a5cln, a1==2 & (is.na(a5) | a5==2), 1),
        a7 = replace(a7, a7==77 | a7==88 | a7==99, NA),
        a7cln = if_else(a7>=1 & a7<=50, 1, 2, missing = 2),
        cln = if_else(a1cln==1 & a2cln==1 & a5cln==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, a1==1 & a2==1 & a5==1 & a7cln==2, 2),
        cln = replace(cln, (a5==2 | is.na(a5)) & a7>1, 2),
        clndrinker = if_else(a1==1 & a2==1 & a5==1 & a7cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          a7<4 ~ "3) lower-end",
          a7>=4 & a7<6 ~ "2) intermed level",
          a7>=6 ~ "1) high-end",
          a5==2 | is.na(a5) ~ "not a current drinker",
          TRUE ~ NA_character_
        ), levels = c("1) high-end", "2) intermed level", "3) lower-end", "not a current drinker")),
        d = factor(case_when(
          a7<2 ~ "3) lower-end",
          a7>=2 & a7<4 ~ "2) intermed level",
          a7>=4 ~ "1) high-end",
          a5==2 | is.na(a5) ~ "not a current drinker",
          TRUE ~ NA_character_
        ), levels = c("1) high-end", "2) intermed level", "3) lower-end", "not a current drinker")),
        e = factor(if_else(sex=="Men", c, d, missing = d),
                   levels=c("1) high-end", "2) intermed level", "3) lower-end", "not a current drinker"))
      )
  }
}
