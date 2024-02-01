################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cethnic <- function(.data) {

  # variable names that are used in the function
  cethnic_names <- c("sex", "c6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cethnic_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cethnic_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(is.na(c6) | c6==88 | c6==77 | valid==2, 2, 1, missing = 1)) %>% 
      # added for KGZ to label output columns
      mutate(c = ifelse(c6==1, "1) Kyrgyz", NA)) %>% 
      mutate(c = replace(c, c6==2, "2) Russian")) %>% 
      mutate(c = replace(c, c6==3, "3) Uzbek")) %>% 
      mutate(c = replace(c, c6==4, "4) Other")) %>% 
      mutate(c = factor(c))
  }

}

