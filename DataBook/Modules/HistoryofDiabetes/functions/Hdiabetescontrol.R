################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hdiabetescontrol <- function(.data) {
  
  hdiabetescontrol_names <- c("sex", "h6", "h7a", "hx1", "hx2", "hx3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetescontrol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetescontrol_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        # HX1. Have you received at least 2 HbA1C (glycated haemoglobin) tests in 
        # the past year as part of your diabetes control?
        hx1cln = if_else(h6==1 & h7a==1 & (hx1==1 | hx1==2) & valid==1, 1, 2, missing = 2),
        # HX2. When was the last time your eyes were examined as part of your diabetes control?
        hx2cln = if_else(h6==1 & h7a==1 & (hx2==1 | hx2==2 | hx2==3) & valid==1, 1, 2, missing = 2),
        # HX3. When was the last time your feet were examined as part of your diabetes control?
        hx3cln = if_else(h6==1 & h7a==1 & (hx3==1 | hx3==2 | hx3==3) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(hx1==1, "1) received at least 2 HbA1C tests",
                          ifelse(hx1==2, "2) has not received at least 2 HbA1C tests", NA)),
                   levels = c("1) received at least 2 HbA1C tests",
                              "2) has not received at least 2 HbA1C tests")),
        d = factor(ifelse(hx2==1, "1) within the past 2 years", 
                          ifelse(hx2==2, "2) more than 2 years ago", 
                                 ifelse(hx2==3, "3) never", NA))),
                   levels = c("1) within the past 2 years", "2) more than 2 years ago", "3) never")),
        e = factor(ifelse(hx3==1, "1) within the last year", 
                          ifelse(hx3==2, "2) more than 1 year ago", 
                                 ifelse(hx3==3, "3) never", NA))),
                   levels = c("1) within the last year", "2) more than 1 year ago", "3) never"))
      )
  }
  
}
