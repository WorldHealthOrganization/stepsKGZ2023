################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpquitting <- function(.data) {
  
  # variable names that are used in the function
  tpquitting_names <- c("sex", "t1", "tp4", "tp5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpquitting_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpquitting_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(tp5==1, "1) thought about quitting", 
                          ifelse(tp5==2, "2) did not think about quitting", NA)),
                   levels = c("1) thought about quitting", "2) did not think about quitting")),
        cln = if_else(t1==1 & tp4==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
