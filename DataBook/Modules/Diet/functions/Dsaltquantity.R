################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dsaltquantity <- function(.data) {
  
  # variable names that are used in the function
  dsaltquantity_names <- c("sex", "d8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dsaltquantity_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dsaltquantity_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(d8==1 | d8==2, "1) eat far too much or too much salt", 
                          ifelse(d8==3 | d8==4 | d8==5, "2) eat right amount or less salt", NA)),
                   levels = c("1) eat far too much or too much salt",
                              "2) eat right amount or less salt")),
        # d = ifelse(d8==1, "1) eat far too much salt", NA),
        # d = ifelse(d8==2, "2) eat too much salt", d),
        # d = ifelse(d8==3, "3) eat right amount of salt", d),
        # d = ifelse(d8==4, "4) eat too little salt", d),
        # d = ifelse(d8==5, "5) eat far too little salt", d),
        # d = factor(d),
        d = factor(case_when(
          d8==1 ~ "1) eat far too much salt",
          d8==2 ~ "2) eat too much salt",
          d8==3 ~ "3) eat right amount of salt",
          d8==4 ~ "4) eat too little salt",
          d8==5 ~ "5) eat far too little salt",
          TRUE ~ NA_character_
        ), levels = c(
          "1) eat far too much salt",
          "2) eat too much salt",
          "3) eat right amount of salt",
          "4) eat too little salt",
          "5) eat far too little salt"
        )),
        cln = if_else(valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(c), 2)
      )
  }
  
}
