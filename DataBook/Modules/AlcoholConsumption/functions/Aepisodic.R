################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

aepisodic <- function(.data) {
  
  aepisodic_names <- c("sex", "a1", "a2", "a5", "a9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aepisodic_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aepisodic_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2),
        a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2),
        a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1),
        a5cln = if_else(a2==1 & (a5==1 | a5==2), 1, 2, missing = 2),
        a5cln = replace(a5cln, a2==2 & (is.na(a5) | a5==2), 1),
        a5cln = replace(a5cln, a1==2 & (is.na(a5) | a5==2), 1),
        a9 = replace(a9, a9==77 | a9==88 | a9==99, NA),
        a9cln = if_else(is.na(a9) | a9>30, 2, 1, missing = 1),
        cln = if_else(a1cln==1 & a2cln==1 & a5cln==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, cln==1 & (a5==2 | is.na(a5)) & a9>0, 2),
        clndrinker = if_else(a1==1 & a2==1 & a5==1 & a9cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(if_else(a9>=1, "1) drank 6+ drinks at least once", "2) did not drink 6+ drinks", 
                           missing = "2) did not drink 6+ drinks"),
                   levels=c("1) drank 6+ drinks at least once","2) did not drink 6+ drinks"))
      )
  }
  
}
