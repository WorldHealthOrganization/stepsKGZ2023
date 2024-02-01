################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokestatus <- function(.data) {
  
  # variable names that are used in the function
  tsmokestatus_names <- c("sex", "t1", "t2", "t8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokestatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokestatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 2, missing = 2),
        t2cln = replace(t2cln, t1==2 & (is.na(t2) | t2==2), 1),
        t8cln = if_else(t1==2 & (t8==1 | t8==2), 1, 2, missing = 2),
        t8cln = replace(t8cln, t1==1 & (is.na(t8) | t8==1), 1),
        cln = if_else(t1cln==1 & t2cln==1 & t8cln==1 & valid==1, 1, 2, missing = 2),
        # Smoking status
        c = factor(case_when(
          t1==1 & t2==2 ~ "2) current smoker (non-daily)",
          t1==1 & t2==1 ~ "1) daily smoker",
          t1==2 & t8==1 ~ "3) former smoker",
          t8==2 ~ "4) never smoked",
          TRUE ~ NA_character_
        ), levels = c(
          "1) daily smoker", "2) current smoker (non-daily)",
          "3) former smoker", "4) never smoked"
        )),
        # Percentage of current smokers
        d = factor(if_else(t1==1 & (t2==1 | t2==2), "1) daily and non-daily smokers", 
                           "2) non-smoker", missing = "2) non-smoker"),
                   levels = c("1) daily and non-daily smokers", "2) non-smoker"))
      )
  }
  
}
