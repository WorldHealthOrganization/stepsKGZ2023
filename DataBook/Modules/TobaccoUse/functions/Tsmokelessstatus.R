################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokelessstatus <- function(.data) {
  
  # variable names that are used in the function
  tsmokelessstatus_names <- c("sex", "t12", "t13", "t15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokelessstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokelessstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t12cln = if_else(t12==1 | t12==2, 1, 2, missing = 2),
        t13cln = if_else(t12==1 & (t13==1 | t13==2), 1,
                         if_else(t12==2 & (is.na(t13) | t13==2), 1, 2, missing = 2)),
        t15cln = if_else(t12==2 & (t15==1 | t15==2), 1,
                         if_else(t12==1 & (is.na(t15) | t15==1), 1, 2, missing = 2)),
        cln = if_else(t12cln==1 & t13cln==1 & t15cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          t12==1 & t13==2 ~ "2) current user (non-daily)",
          t12==1 & t13==1 ~ "1) daily user",
          t12==2 & t15==1 ~ "3) past user",
          t15==2 ~ "4) never used",
          TRUE ~ NA_character_
        ), levels = c(
          "1) daily user", "2) current user (non-daily)", 
          "3) past user", "4) never used"
        )),
        d = factor(if_else(t12==1 & (t13==1 | t13==2), 
                           "1) daily and non-daily users", "2) non-user", 
                           missing = "2) non-user"),
                   levels = c("1) daily and non-daily users", "2) non-user"))
      )
  }
  
}
