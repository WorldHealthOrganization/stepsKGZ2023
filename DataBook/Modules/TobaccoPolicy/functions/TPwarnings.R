################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpwarnings <- function(.data) {
  
  # variable names that are used in the function
  tpwarnings_names <- c("sex", "t1", "tp4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpwarnings_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),  
      paste(tpwarnings_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(tp4==1, "1) noticed warnings", 
                          ifelse(tp4==2, "2) did not notice warnings", NA)),
                   levels = c("1) noticed warnings", "2) did not notice warnings")),
        cln = if_else(t1==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }  
}
