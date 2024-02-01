################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

dservings <- function(.data) {
  
  # variable names that are used in the function
  dservings_names <- c("sex", "d1", "d2", "d3", "d4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dservings_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dservings_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        d2 = replace(d2, d1==0 & (is.na(d2) | d2>20), 0),
        d4 = replace(d4, d3==0 & (is.na(d4) | d4>20), 0),
        d2 = replace(d2, d1>=1 & d1<=7 & d2>20, NA),
        d4 = replace(d4, d3>=1 & d3<=7 & d4>20, NA),
        d1cln = if_else(is.na(d1) | d1>7, 2, 1, missing = 1),
        d3cln = if_else(is.na(d3) | d3>7, 2, 1, missing = 1),
        d2cln = if_else(d1cln==1 & d1>0 & d2>0, 1, 2, missing = 2),
        d4cln = if_else(d3cln==1 & d3>0 & d4>0, 1, 2, missing = 2),
        d2cln = replace(d2cln, d1==0 & d2==0, 1),
        d4cln = replace(d4cln, d3==0 & d4==0, 1),
        fruitcln = if_else(d1cln==1 & d2cln==1 & valid==1, 1, 2, missing = 2),
        vegcln = if_else(d3cln==1 & d4cln==1 & valid==1, 1, 2, missing = 2),
        fservings = ifelse(fruitcln==1, (d1*d2)/7, NA),
        vservings = ifelse(vegcln==1, (d3*d4)/7, NA),
        # Create combined field for fruit and veg for number of days paying 
        # attention to the CLN variable
        fvservings = ifelse(fruitcln==1 & vegcln==1, fservings + vservings, NA),
        fvservings = ifelse(fruitcln==1 & vegcln==2, fservings, fvservings),
        fvservings = ifelse(fruitcln==2 & vegcln==1, vservings, fvservings),
        fvcln = if_else((fruitcln==1 | vegcln==1) & valid==1, 1, 2, missing = 2)
      )
  }
  
}
