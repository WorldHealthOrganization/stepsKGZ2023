################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

etsmokeagetime <- function(.data) {
  
  # variable names that are used in the function
  etsmokeagetime_names <- c("sex", "et1", "et2", "et3", "et4", "et4type")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, etsmokeagetime_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(etsmokeagetime_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        et1cln = if_else(et1==1 | et1==2, 1, 2, missing = 2),
        et2cln = if_else(et1==1 & (et2==1 | et2==2), 1, 2, missing = 2),
        et2cln = replace(et2cln, et1==2 & (is.na(et2) | et2==2), 1),
        et4a = ifelse(et4type=="years", et4, NA),
        et4byrs = ifelse(et4type=="months", et4/12, NA),
        et4cyrs = ifelse(et4type=="weeks", et4/52, NA),
        et3cln = if_else(et3>=7 & et3<70, 1, 2, missing = 2),
        et4acln = if_else(et4a>0 & et4a<=61, 1, 2, missing = 2),
        et4bcln = if_else(is.na(et4byrs), 2, 1, missing = 1),
        et4ccln = if_else(is.na(et4cyrs), 2, 1, missing = 1),
        initiation = ifelse(et4ccln==1, age-et4cyrs, 
                            ifelse(et4bcln==1, age-et4byrs,
                                   ifelse(et4acln==1, age-et4a, 
                                          ifelse(et3cln==1, et3, NA)))),
        duration = age - initiation,
        cln = if_else(et1cln==1 & et2cln==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, initiation<7 | is.na(initiation) | duration<0, 2)
      )
  }
}
