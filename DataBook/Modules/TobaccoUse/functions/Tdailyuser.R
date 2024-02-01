################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tdailyuser <- function(.data) {
  
  # variable names that are used in the function
  tdailyuser_names <- c("sex", "t1", "t2", "t8", "t12", "t13", "t15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tdailyuser_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tdailyuser_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 
                        if_else(t1==2 & (is.na(t2) | t2==2), 1, 2, missing = 2)),
        t8cln = if_else(t1==2 & (t8==1 | t8==2), 1, 
                        if_else(t1==1 & (is.na(t8) | t8==1), 1, 2, missing = 2)),
        t12cln = if_else(t12==1 | t12==2, 1, 2, missing = 2),
        t13cln = if_else(t12==1 & (t13==1 | t13==2), 1, 
                         if_else(t12==2 & (is.na(t13) | t13==2), 1, 2, missing = 2)),
        t15cln = if_else(t12==2 & (t15==1 | t15==2), 1, 
                         if_else(t12==1 & (is.na(t15) | t15==1), 1, 2, missing = 2)),
        cln = if_else(t1cln==1 & t2cln==1 & t8cln==1 & t12cln==1 & t13cln==1 & 
                        t15cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(if_else(t1==1 | t12==1, "1) current tobacco user", "2) not current tobacco user", 
                           missing = "2) not current tobacco user"),
                   levels = c("1) current tobacco user", "2) not current tobacco user")),
        d = factor(if_else((t1==1 & t2==1) | (t12==1 & t13==1), "1) current daily user", 
                           "2) not current daily user", missing = "2) not current daily user"),
                   levels = c("1) current daily user", "2) not current daily user")),
        # next var e isn't used in Data Book
        e = factor(if_else(t1==1 | t12==1, "1) current tobacco user", 
                           if_else(t1==2 & t12==2 & (t8==1 | t15==1), "2) past tobacco user",
                                   if_else(t1==2 & t12==2 & t8==2 & t15==2, "3) never user", NA))),
                   levels = c("1) current tobacco user", "2) past tobacco user", "3) never user"))
      )
  }
}


