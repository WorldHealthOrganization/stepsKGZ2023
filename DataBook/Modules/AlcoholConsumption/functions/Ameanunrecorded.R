################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

ameanunrecorded <- function(.data) {
  
  ameanunrecorded_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", "a10d", "a10e", 
                             "a10f", "a10g", "a11", "a12a", "a12b", "a12c", "a12d", "a12e")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ameanunrecorded_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ameanunrecorded_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        allmissing = if_else((is.na(a10a) | a10a>=77) & (is.na(a10b) | a10b>=77) 
                             & (is.na(a10c) | a10c>=77), 1, 2, missing = 2),
        allmissing = if_else(allmissing==1 & (is.na(a10d) | a10d>=77) & (is.na(a10e) | a10e>=77) 
                             & (is.na(a10f) | a10f>=77) & (is.na(a10g) | a10g>=77), 1, 2, missing = 2),
        a10a = replace(a10a, allmissing==2 & (is.na(a10a) | a10a>=77), 0),
        a10b = replace(a10b, allmissing==2 & (is.na(a10b) | a10b>=77), 0),
        a10c = replace(a10c, allmissing==2 & (is.na(a10c) | a10c>=77), 0),
        a10d = replace(a10d, allmissing==2 & (is.na(a10d) | a10d>=77), 0),
        a10e = replace(a10e, allmissing==2 & (is.na(a10e) | a10e>=77), 0),
        a10f = replace(a10f, allmissing==2 & (is.na(a10f) | a10f>=77), 0),
        a10g = replace(a10g, allmissing==2 & (is.na(a10g) | a10g>=77), 0),
        totalpastweek = a10a+a10b+a10c+a10d+a10e+a10f+a10g,
        cln = if_else(a1==1 & a2==1 & a5==1 & a11==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, a11==1 & totalpastweek==0, 2),
        total = 0,
        onecln = 2,
        total = ifelse(a12a>=0 & a12a<=50, total+a12a, total==total),
        onecln = ifelse(a12a>=0 & a12a<=50, 1, total==total),
        total = ifelse(a12b>=0 & a12b<=50, total+a12b, total==total),
        onecln = ifelse(a12b>=0 & a12b<=50, 1, total==total),
        total = ifelse(a12c>=0 & a12c<=50, total+a12c, total==total),
        onecln = ifelse(a12c>=0 & a12c<=50, 1, total==total),
        total = ifelse(a12d>=0 & a12d<=50, total+a12d, total==total),
        onecln = ifelse(a12d>=0 & a12d<=50, 1, total==total),
        total = ifelse(a12e>=0 & a12e<=50, total+a12e, total==total),
        onecln = ifelse(a12e>=0 & a12e<=50, 1, total==total),
        total = ifelse(is.na(total), 0, total),
        avgperday = total/7,
        onecln = replace(onecln, total==0, 2),
        cln = replace(cln, totalpastweek<total, 2)
      )
  }
  
}
