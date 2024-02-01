################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

astopdrink <- function(.data) {
  
  astopdrink_names <- c("sex", "a1", "a2", "a3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, astopdrink_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(astopdrink_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2),
        a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2),
        a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1),
        a3cln = if_else((a3==1 | a3==2) & a1==1 & a2==2, 1, 2, missing = 2),
        cln = if_else(a1cln==1 & a2cln==1 & a3cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(a3==1, "1) stopped due to health reasons", 
                          ifelse(a3==2, "2) did not stop due to health reasons", NA)),
                   levels = c("1) stopped due to health reasons",
                              "2) did not stop due to health reasons"))
      )
  }
  
}
