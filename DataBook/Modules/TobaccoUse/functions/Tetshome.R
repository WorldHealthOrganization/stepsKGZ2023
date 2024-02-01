################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tetshome <- function(.data) {
  
  # variable names that are used in the function
  tetshome_names <- c("sex", "t17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tetshome_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tetshome_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((t17==1 | t17==2) & valid==1, 1, 2, missing = 2),
        c = factor(if_else(t17==1, "exposed at home", 
                           if_else(t17==2, "not exposed", NA)),
                   levels = c("exposed at home", "not exposed"))
      )
  }
}
