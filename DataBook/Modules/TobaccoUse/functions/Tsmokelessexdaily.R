################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmokelessexdaily <- function(.data) {
  
  # variable names that are used in the function
  tsmokelessexdaily_names <- c("sex", "t12", "t13", "t15", "t16")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokelessexdaily_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokelessexdaily_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t12cln = if_else(t12==1 | t12==2, 1, 2, missing = 2), 
        t13cln = if_else(t12==1 & (t13==1 | t13==2), 1,
                         if_else(t12==2 & (is.na(t13) | t13==2), 1, 2, missing = 2)),
        t15cln = if_else(t12==2 & (t15==1 | t15==2), 1, 
                         if_else(t12==1 & (is.na(t15) | t15==1), 1, 2, missing = 2)),
        cln = if_else(t12cln==1 & t13cln==1 & t15cln==1 & valid==1, 1, 2, missing = 2),
        t16cln = if_else(t15==2 & (is.na(t16) | t16==2), 1,
                         if_else(t15==1 & (t16==1 | t16==2), 1, 
                                 if_else(t13==1 & is.na(t16), 1,
                                         if_else(t13==1 & t16==1, 2,
                                                 if_else(t13==2 & (t16==2 | t16==1), 1, 2, missing = 2))))),
        cln = replace(cln, t16cln==2, 2),
        everdaily = if_else(t13==1 | t16==1, 1, 2, missing = 2),
        c = ifelse(t16==1, "Ex-daily user", NA),
        c = replace(c, t16==2, "Not an ex-daily user"),
        c = replace(c, is.na(c) & cln==1, "Not an ex-daily user"),
        c = replace(c, t13==1, "Not an ex-daily user"),
        c = factor(c, levels = c("Ex-daily user", "Not an ex-daily user"))
      )
  }
  
}
