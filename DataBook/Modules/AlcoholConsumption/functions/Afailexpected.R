################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

afailexpected <- function(.data) {
  
  afailexpected_names <- c("sex", "a1", "a2", "a14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afailexpected_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afailexpected_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(a1==1 & a2==1 & a14>=1 & a14<=5 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(a14==1 | a14==2 | a14==3, "1) monthly or more frequently",
                          ifelse(a14==4, "2) less than monthly",
                                 ifelse(a14==5, "3) never", NA))),
                   levels=c("1) monthly or more frequently",
                            "2) less than monthly",
                            "3) never"))
      )
  }
  
}
