################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokeagetime <- function(.data) {
  
  # variable names that are used in the function
  tsmokeagetime_names <- c("sex", "t1", "t2", "t3", "t4", "t4type")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokeagetime_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokeagetime_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 
                        if_else(t1==2 & (is.na(t2) | t2==2), 1, 2, missing = 2)),
        t4a = ifelse(t4type=="years", t4, NA),
        t4byrs = ifelse(t4type=="months", t4/12, NA),
        t4cyrs = ifelse(t4type=="weeks", t4/52, NA),
        t3cln = if_else(t3>=7 & t3<70, 1, 2, missing = 2),
        t4acln = if_else(t4a>0 & t4a<=61, 1, 2, missing = 2),
        t4bcln = if_else(is.na(t4byrs), 2, 1, missing = 1),
        t4ccln = if_else(is.na(t4cyrs), 2, 1, missing = 1),
        initiation = ifelse(t4ccln==1, age-t4cyrs, 
                            ifelse(t4bcln==1, age-t4byrs,
                                   ifelse(t4acln==1, age-t4a, 
                                          ifelse(t3cln==1, t3, NA)))),
        duration = age - initiation,
        cln = if_else(t1cln==1 & t2cln==1 & valid==1, 1, 
                      if_else(initiation<7 | is.na(initiation) | duration<0, 2, 2, missing = 2))
      )
  }
}
