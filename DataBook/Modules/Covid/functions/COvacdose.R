################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

covacdose <- function(.data) {
  
  covacdose_names <- c("sex", "co1", "co2")
  
  if(!all(i <- rlang::has_name(.data, covacdose_names))) {
    stop(sprintf("%s doesn't contain: %s",  
                 deparse(substitute(.data)),
                 paste(covacdose_names[!i], collapse=", ")))
  } else {
    
    .data %>%
      mutate(
        cln = if_else(co1==1 & (co2==1 | co2==2 | co2==3) & valid==1, 1, 2, missing = 2),
        c = factor(case_when(
          co2==1 ~ "1) 1 dose",  
          co2==2 ~ "2) 2 doses",
          co2==3 ~ "3) 3 or more doses",
          TRUE ~ NA_character_
        ),
        levels = c("1) 1 dose", "2) 2 doses", "3) 3 or more doses"))
      )
    
  }
}
