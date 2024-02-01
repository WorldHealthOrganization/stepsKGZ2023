################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coservice <- function(.data) {
  
  # variable names that are used in the function
  coservice_names <- c("sex", "co5", "co6a", "co6b", "co6c")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coservice_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coservice_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        # CO6a. Primary healthcare services
        co6acln = if_else((co6a==1 | co6a==2) & co5==1 & valid==1, 1, 2, missing = 2),
        a = factor(ifelse(co6a==1, "1) utilized", 
                          ifelse(co6a==2, "2) didn't utilize", NA)),
                   levels = c("1) utilized", "2) didn't utilize")),
        # CO6b. Specialized healthcare services (e.g., hospital)
        co6bcln = if_else((co6b==1 | co6b==2) & co5==1 & valid==1, 1, 2, missing = 2),
        b = factor(ifelse(co6b==1, "1) utilized", 
                          ifelse(co6b==2, "2) didn't utilize", NA)),
                   levels = c("1) utilized", "2) didn't utilize")),
        # CO6c. Other healthcare services
        co6ccln = if_else((co6c==1 | co6c==2) & co5==1 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(co6c==1, "1) utilized", 
                          ifelse(co6c==2, "2) didn't utilize", NA)),
                   levels = c("1) utilized", "2) didn't utilize"))
      )
  }
  
}

