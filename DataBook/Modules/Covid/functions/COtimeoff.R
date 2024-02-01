################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cotimeoff <- function(.data) {
  
  cotimeoff_names <- c("sex", "co5", "co8", "co8a")
  
  if(!all(i <- rlang::has_name(.data, cotimeoff_names))) {
    stop(sprintf("%s doesn't contain: %s", 
                 deparse(substitute(.data)), 
                 paste(cotimeoff_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((co8==1 | co8==2 | co8==3) &  co5==1 & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          co8==1 ~ "1) Yes",
          co8==2 ~ "2) No", 
          co8==3 ~ "3) Don’t work or study",
          TRUE ~ NA_character_
        ), levels = c("1) Yes", "2) No", "3) Don’t work or study")),
        # convert into days for average 
        co8adays = case_when(
          co8a_type==1 ~ co8a, # days
          co8a_type==2 ~ co8a*7, # weeks
          co8a_type==3 ~ co8a*30, # months
          co8a_type==4 ~ co8a*365, # years
          TRUE ~ NA_real_  
        ),
        # exclude NA and valid==2
        co8acln = if_else(is.na(co8a) | valid==2, 2, 1, missing = 1)
      )
  }
}
