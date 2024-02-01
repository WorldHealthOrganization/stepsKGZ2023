################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

apastweek <- function(.data) {
  
  apastweek_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", "a10d", "a10e", "a10f", "a10g")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, apastweek_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(apastweek_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        allmissing = if_else((is.na(a10a) | a10a>=77) & (is.na(a10b) | a10b>=77)
                             & (is.na(a10c) | a10c>=77) & (is.na(a10d) | a10d>=77)
                             & (is.na(a10e) | a10e>=77) & (is.na(a10f) | a10f>=77)
                             & (is.na(a10g) | a10g>=77), 1, 2, missing = 2),
        a10a = replace(a10a, allmissing==2 & (is.na(a10a) | a10a>=77), 0),
        a10b = replace(a10b, allmissing==2 & (is.na(a10b) | a10b>=77), 0),
        a10c = replace(a10c, allmissing==2 & (is.na(a10c) | a10c>=77), 0),
        a10d = replace(a10d, allmissing==2 & (is.na(a10d) | a10d>=77), 0),
        a10e = replace(a10e, allmissing==2 & (is.na(a10e) | a10e>=77), 0),
        a10f = replace(a10f, allmissing==2 & (is.na(a10f) | a10f>=77), 0),
        a10g = replace(a10g, allmissing==2 & (is.na(a10g) | a10g>=77), 0),
        a10cln = if_else(a10a<51 & a10b<51 & a10c<51 & a10d<51 & a10e<51 & a10f<51
                         & a10g<51, 1, 2, missing = 2),
        cln = if_else(a1==1 & a2==1 & a5==1 & a10cln==1 & valid==1, 1, 2, missing = 2),
        a10acount = if_else(a10a>0 & a10a<51, 1, 0, missing = 0),
        a10bcount = if_else(a10b>0 & a10b<51, 1, 0, missing = 0),
        a10ccount = if_else(a10c>0 & a10c<51, 1, 0, missing = 0),
        a10dcount = if_else(a10d>0 & a10d<51, 1, 0, missing = 0),
        a10ecount = if_else(a10e>0 & a10e<51, 1, 0, missing = 0),
        a10fcount = if_else(a10f>0 & a10f<51, 1, 0, missing = 0),
        a10gcount = if_else(a10g>0 & a10g<51, 1, 0, missing = 0),
        days = a10acount+a10bcount+a10ccount+a10dcount+a10ecount+a10fcount+a10gcount,
        c = factor(case_when(
          days==7 ~ "1) daily",
          days==5 | days==6 ~ "2) 5-6 days",
          days==3 | days==4 ~ "3) 3-4 days",
          days==1 | days==2 ~ "4) 1-2 days",
          days==0 ~ "5) 0 days",
          TRUE ~ NA_character_
        ), levels = c("1) daily", "2) 5-6 days", "3) 3-4 days", "4) 1-2 days", "5) 0 days")),
        meandrinksperday = (a10a+a10b+a10c+a10d+a10e+a10f+a10g)/7
      )
  }
}
