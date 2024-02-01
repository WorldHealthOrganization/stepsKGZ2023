################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpcost <- function(.data) {
  
  # variable names that are used in the function
  tpcost_names <- c("sex", "t1", "t2", "t5a", "t5aw", "tp6", "tp7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpcost_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)), 
      paste(tpcost_names[!i], collapse=", ")
    ))
  } else {  
    .data %>%
      mutate(
        tp6cln = if_else(tp6>0 & tp6<7777, 1, 2, missing = 2),
        tp7cln = if_else(tp7>0 & tp7<7777, 1, 2, missing = 2),
        priceper20cigs = (tp7/tp6) * 20,
        cln = if_else(t1==1 & tp6cln==1 & tp7cln==1 & valid==1, 1, 2, missing = 2),
        cigcln = cln,
        cigcln = replace(cigcln, t2==1 & (is.na(t5a) | t5a>50), 2),
        cigcln = replace(cigcln, t2==2 & (is.na(t5aw) | t5aw>350), 2),
        cigcln = replace(cigcln, t5a>50 | t5aw>350, 2),
        cigspermonth = ifelse(t1==1 & t5aw>0 & t5aw<=350, t5aw * 4, 
                              ifelse(t2==1 & t5a>0 & t5a<=50, t5a * 30, NA)),
        monthlyexp = cigspermonth * (tp7/tp6)
      )
  }
}
