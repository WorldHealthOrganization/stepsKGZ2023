################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

etstatus <- function(.data) {
  
  # variable names that are used in the function
  etstatus_names <- c("sex", "et1", "et2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, etstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(etstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        et1cln = if_else(et1==1 | et1==2, 1, 2, missing = 2),
        et2cln = if_else(et1==1 & (et2==1 | et2==2), 1, 2, missing = 2),
        et2cln = replace(et2cln, et1==2 & (is.na(et2) | et2==2), 1),
        cln = if_else(et1cln==1 & et2cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          et1==1 & et2==2 ~ "2) current user (non-daily)",
          et1==1 & et2==1 ~ "1) current daily user",
          et1==2 ~ "3) former user or never used",
          TRUE ~ NA_character_
        ), levels = c(
          "1) current daily user", "2) current user (non-daily)",
          "3) former user or never used"
        )),
        d = factor(if_else(et1==1 & (et2==1 | et2==2), "1) daily and non-daily users", 
                           "2) non-users", missing = "2) non-users"),
                   levels = c("1) daily and non-daily users", "2) non-users"))
      )
  }
}

