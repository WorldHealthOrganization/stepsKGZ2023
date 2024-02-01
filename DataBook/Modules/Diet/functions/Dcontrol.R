################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dcontrol <- function(.data) {
  
  # variable names that are used in the function
  dcontrol_names <- c("sex", "d11a", "d11b", "d11c", "d11d", "d11e", "d11f")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dcontrol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dcontrol_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        a = factor(ifelse(d11a==1, "1) limit consumption of processed foods", 
                          ifelse(d11a==2, "2) did not limit consumption of processed foods", NA)),
                   levels = c("1) limit consumption of processed foods", 
                              "2) did not limit consumption of processed foods")),
        b = factor(ifelse(d11b==1, "1) looked at salt labels on food", 
                          ifelse(d11b==2, "2) did not look at salt labels on food", NA)),
                   levels = c("1) looked at salt labels on food",
                              "2) did not look at salt labels on food")),
        c = factor(ifelse(d11c==1, "1) bought low salt alternatives", 
                          ifelse(d11c==2, "2) did not buy low salt alternatives", NA)),
                   levels = c("1) bought low salt alternatives",
                              "2) did not buy low salt alternatives")),
        d = factor(ifelse(d11d==1, "1) used other spices when cooking", 
                          ifelse(d11d==2, "2) did not use other spices when cooking", NA)),
                   levels = c("1) used other spices when cooking", 
                              "2) did not use other spices when cooking")),
        e = factor(ifelse(d11e==1, "1) avoided eating out", 
                          ifelse(d11e==2, "2) did not avoid eating out", NA)),
                   levels = c("1) avoided eating out", "2) did not avoid eating out")),
        f = factor(ifelse(d11f==1, "1) did some other behavior", 
                          ifelse(d11f==2, "2) did not do some other behavior", NA)),
                   levels = c("1) did some other behavior", "2) did not do some other behavior")),
        d11acln = if_else(valid==1, 1, 2, missing = 2),
        d11acln = replace(d11acln, is.na(a), 2),
        d11bcln = if_else(valid==1, 1, 2, missing = 2),
        d11bcln = replace(d11bcln, is.na(b), 2),
        d11ccln = if_else(valid==1, 1, 2, missing = 2),
        d11ccln = replace(d11ccln, is.na(c), 2),
        d11dcln = if_else(valid==1, 1, 2, missing = 2),
        d11dcln = replace(d11dcln, is.na(d), 2),
        d11ecln = if_else(valid==1, 1, 2, missing = 2),
        d11ecln = replace(d11ecln, is.na(e), 2),
        d11fcln = if_else(valid==1, 1, 2, missing = 2),
        d11fcln = replace(d11fcln, is.na(f), 2)
      )
  }
  
}
