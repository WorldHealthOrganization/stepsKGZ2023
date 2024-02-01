################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

anotabletostop <- function(.data) {
  
  anotabletostop_names <- c("sex", "a1", "a2", "a13")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, anotabletostop_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(anotabletostop_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        c = factor(ifelse(a13==1 | a13==2 | a13==3, "1) monthly or more frequently",
                          ifelse(a13==4, "2) less than monthly",
                                 ifelse(a13==5, "3) never", NA))),
                   levels=c("1) monthly or more frequently",
                            "2) less than monthly", 
                            "3) never")),
        cln = if_else(a1==1 & a2==1 & a13>=1 & a13<=5 & valid==1, 1, 2, missing = 2)
      )
  }
  
}
