################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mbloodpressure <- function(.data) {
  
  # variable names that are used in the function
  mbloodpressure_names <- c("sex", "m4a", "m4b", "m5a", "m5b", "m6a", "m6b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mbloodpressure_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mbloodpressure_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2),
        m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2),
        m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2),
        m4bcln = if_else(m4b>=30 & m4b<=200, 1, 2, missing = 2),
        m5bcln = if_else(m5b>=30 & m5b<=200, 1, 2, missing = 2),
        m6bcln = if_else(m6b>=30 & m6b<=200, 1, 2, missing = 2),
        sbp = if_else((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1, (m5a+m6a)/2, 
                      if_else(m4acln==1 & m5acln==2 & m6acln==1, (m4a+m6a)/2, 
                              if_else(m4acln==1 & m5acln==1 & m6acln==2, (m4a+m5a)/2, NA))),
        dbp = if_else((m4bcln==1 | m4bcln==2) & m5bcln==1 & m6bcln==1, (m5b+m6b)/2, 
                      if_else(m4bcln==1 & m5bcln==2 & m6bcln==1, (m4b+m6b)/2, 
                              if_else(m4bcln==1 & m5bcln==1 & m6bcln==2, (m4b+m5b)/2, NA))),
        sbpcln = if_else((sbp<40 | sbp>300) | is.na(sbp) | valid==2, 2, 1, missing = 2),
        dbpcln = if_else((dbp<30 | dbp>200) | is.na(dbp) | valid==2, 2, 1, missing = 2)
      )
  }
  
}
