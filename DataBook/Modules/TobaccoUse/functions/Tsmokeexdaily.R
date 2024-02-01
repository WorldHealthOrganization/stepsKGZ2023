################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokeexdaily <- function(.data) {
  
  # variable names that are used in the function
  tsmokeexdaily_names <- c("sex", "t1", "t2", "t8", "t9", "t10", "t11", "t11type")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokeexdaily_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokeexdaily_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        t2cln = if_else(t1==1 & (t2==1 | t2==2), 1,
                        if_else(t1==2 & (is.na(t2) | t2==2), 1, 2, missing = 2)),
        t8cln = if_else(t1==2 & (t8==1 | t8==2), 1,
                        if_else(t1==1 & (is.na(t8) | t8==1), 1, 2, missing = 2)),
        cln = if_else(t1cln==1 & t2cln==1 & t8cln==1 & valid==1, 1, 2, missing = 2),
        t9cln = if_else(t8==2 & (is.na(t9) | t9==2), 1, 
                        if_else(t8==1 & (t9==1 | t9==2), 1, 
                                if_else(t2==1 & is.na(t9), 1, 
                                        if_else(t2==1 & t9==1, 2,
                                                if_else(t2==2 & (t9==2 | t9==1), 1, 2, missing = 2))))),
        cln = replace(cln, t9cln==2, 2),
        # Calculate T11 (months) (weeks) years
        t11a = ifelse(t11type=="years", t11, NA),
        t11byrs = ifelse(t11type=="months", t11/12, NA),
        t11cyrs = ifelse(t11type=="weeks", t11/52, NA), # UPDATE 2024-01-09: corrected to t11/52
        t10cln = if_else(t10>=7 & t10<=69, 1, 2, missing = 2),
        t11acln = if_else(t11a>0 & t11a<=61, 1, 2, missing = 2),
        t11bcln = if_else(is.na(t11byrs), 2, 1, missing = 1),
        t11ccln = if_else(is.na(t11cyrs), 2, 1, missing = 1),
        # Order the priority of responses so that T10 takes precedence over other responses
        stop = ifelse(t11ccln==1, t11cyrs,
                      ifelse(t11bcln==1, t11byrs,
                             ifelse(t11acln==1, t11a,
                                    ifelse(t10cln==1, age-t10, NA)))),
        stopcln = if_else(is.na(stop) | stop>61, 2, 1, missing = 1),
        c = ifelse(t9==1, "Ex-daily smoker", NA),
        c = replace(c, t9==2, "Not an ex-daily smoker"),
        c = replace(c, is.na(c) & cln==1, "Not an ex-daily smoker"),
        c = replace(c, t2==1, "Not an ex-daily smoker"),
        c = factor(c, levels = c("Ex-daily smoker", "Not an ex-daily smoker")),
        everdaily = if_else(t2==1 | t9==1, 1, 2, missing = 2)
      )
  }
  
}
