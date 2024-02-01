################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tcessation <- function(.data) {
  
  # variable names that are used in the function
  tcessation_names <- c("sex", "t1", "t2", "t6", "t7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tcessation_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tcessation_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(t1==1 & (t2==1 | t2==2) & valid==1, 1, 2, missing = 2),
        stopcln = if_else((t6==1 | t6==2) & cln==1, 1, 2, missing = 2),
        mdcln = if_else((t7==1 | t7==2) & cln==1, 1, 2, missing = 2),
        c = factor(if_else(t6==1, "1) tried to stop smoking", "2) didn't try to stop smoking", 
                           missing = "2) didn't try to stop smoking"), 
                   levels=c("1) tried to stop smoking", "2) didn't try to stop smoking")),
        d = factor(if_else(t7==1, "1) advised to quit", "2) not advised to quit", 
                           missing = "2) not advised to quit"), 
                   levels=c("1) advised to quit", "2) not advised to quit"))
      )
  }
}
