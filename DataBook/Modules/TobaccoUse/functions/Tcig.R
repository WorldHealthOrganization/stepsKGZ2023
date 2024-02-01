################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tcig <- function(.data) {
  
  # variable names that are used in the function
  tcig_names <- c("sex", "t1", "t2", "t5a", "t5b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tcig_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tcig_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(t1==1 & t2==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, t5a>50 | is.na(t5a) | t5b>50 | is.na(t5b), 2),
        total = t5a + t5b,
        c = factor(case_when(
          total>=0 & total<=4 ~ "1) <5 cigs",
          total>=5 & total<=9 ~ "2) 5-9 cigs",
          total>=10 & total<=14 ~ "3) 10-14 cigs",
          total>=15 & total<=24 ~ "4) 15-24 cigs",
          total>=25 & total<=100 ~ "5) >= 25 cigs",
          TRUE ~ NA_character_
        ), levels = c("1) <5 cigs", "2) 5-9 cigs", "3) 10-14 cigs",
                      "4) 15-24 cigs", "5) >= 25 cigs")),
        cln = replace(cln, total==0, 2)
      )
  }
}


