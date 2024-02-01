################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpdanger <- function(.data) {
  
  # variable names that are used in the function
  tpdanger_names <- c("sex", "tp1a", "tp1b", "tp1c", "tp1d") 
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpdanger_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),  
      paste(tpdanger_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        c = factor(ifelse(tp1a==1, "1) noticed information in newspapers", 
                          ifelse(tp1a==2, "2) did not notice information in newspapers", NA)),
                   levels = c("1) noticed information in newspapers", 
                              "2) did not notice information in newspapers")),
        d = factor(ifelse(tp1b==1, "1) noticed information on TV", 
                          ifelse(tp1b==2, "2) did not notice information on TV", NA)),
                   levels = c("1) noticed information on TV", "2) did not notice information on TV")), 
        e = factor(ifelse(tp1c==1, "1) noticed information on radio",
                          ifelse(tp1c==2, "2) did not notice information on radio", NA)),
                   levels = c("1) noticed information on radio", "2) did not notice information on radio")),
        f = factor(ifelse(tp1b==1 | tp1c==1, "1) noticed information on TV or radio", 
                          ifelse(tp1b==2 & tp1c==2, "2) did not notice information on TV or radio", NA)),
                   levels = c("1) noticed information on TV or radio", 
                              "2) did not notice information on TV or radio")),
        g = factor(ifelse(tp1d==1, "1) noticed information on social media channels",  
                          ifelse(tp1d==2, "2) did not notice information on social media channels", NA)),
                   levels = c("1) noticed information on social media channels",
                              "2) did not notice information on social media channels")),
        tp1acln = if_else(valid==1, 1, 2, missing = 2),
        tp1bcln = if_else(valid==1, 1, 2, missing = 2),
        tp1ccln = if_else(valid==1, 1, 2, missing = 2),
        tp1dcln = if_else(valid==1, 1, 2, missing = 2),
        tp1acln = replace(tp1acln, is.na(c), 2),
        tp1bcln = replace(tp1bcln, is.na(d), 2),
        tp1ccln = replace(tp1ccln, is.na(e), 2),
        tp1dcln = replace(tp1dcln, is.na(g), 2)
      )
  }
}
