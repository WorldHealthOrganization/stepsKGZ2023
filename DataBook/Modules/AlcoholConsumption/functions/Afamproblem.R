################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

afamproblem <- function(.data) {
  
  afamproblem_names <- c("sex", "a16")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afamproblem_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afamproblem_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(a16>=1 & a16<=5 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(a16==1 | a16==2, "1) monthly or more frequently",
                          ifelse(a16==3 | a16==4, "2) less than monthly",
                                 ifelse(a16==5, "3) never", NA))),
                   levels = c("1) monthly or more frequently",
                              "2) less than monthly",
                              "3) never"))
      )
  }
  
}
