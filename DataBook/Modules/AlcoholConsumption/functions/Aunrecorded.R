################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

aunrecorded <- function(.data) {
  
  aunrecorded_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", "a10d", 
                         "a10e", "a10f", "a10g", "a11")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aunrecorded_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aunrecorded_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        allmissing = if_else((is.na(a10a) | a10a>=77) & (is.na(a10b) | a10b>=77) 
                             & (is.na(a10c) | a10c>=77) & (is.na(a10d) | a10d>=77) 
                             & (is.na(a10e) | a10e>=77) & (is.na(a10f) | a10f>=77) 
                             & (is.na(a10g) | a10g>=77), 1, 2, missing = 2),
        a10a = replace(a10a, allmissing==2 & (is.na(a10a) | a10a>=77), 0),
        a10b = replace(a10b, allmissing==2 & (is.na(a10b) | a10b>=77), 0),
        a10c = replace(a10c, allmissing==2 & (is.na(a10c) | a10c>=77), 0),
        a10d = replace(a10d, allmissing==2 & (is.na(a10d) | a10d>=77), 0),
        a10e = replace(a10e, allmissing==2 & (is.na(a10e) | a10e>=77), 0),
        a10f = replace(a10f, allmissing==2 & (is.na(a10f) | a10f>=77), 0),
        a10g = replace(a10g, allmissing==2 & (is.na(a10g) | a10g>=77), 0),
        totalpastweek = a10a+a10b+a10c+a10d+a10e+a10f+a10g,
        c = factor(ifelse(a11==1, "consumed unrecorded alcohol", 
                          ifelse(a11==2, "did not consume unrecorded alcohol", NA)),
                   levels = c("consumed unrecorded alcohol",
                              "did not consume unrecorded alcohol")),
        cln = if_else(a1==1 & a2==1 & a5==1 & (a11==1 | a11==2) & valid==1, 1, 2, missing = 2),
        cln = replace(cln, a11==1 & totalpastweek==0, 2)
      )
  }
  
}
