################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

covactime <- function(.data) {
  
  covactime_names <- c("sex", "co2a_dd", "co2a_mm", "co2a_yyyy")
  
  if(!all(i <- rlang::has_name(.data, covactime_names))) {
    stop(sprintf("%s doesn't contain: %s",  
                 deparse(substitute(.data)),
                 paste(covactime_names[!i], collapse=", ")))
  } else {
    
    .data %>%
      mutate(
        cln = if_else(
          (co2a_yyyy==2020 | co2a_yyyy==2021 | co2a_yyyy==2022 | co2a_yyyy==2023) 
          & (co2==1 | co2==2 | co2==3) 
          & valid == 1, 1, 2, missing = 2
        ),
        # for co_time_yyyy
        c = factor(case_when(
          co2a_yyyy==2020 ~ "1) 2020",
          co2a_yyyy==2021 ~ "2) 2021", 
          co2a_yyyy==2022 ~ "3) 2022",
          co2a_yyyy==2023 ~ "4) 2023",  
          TRUE ~ NA_character_
        ),
        levels = c("1) 2020", "2) 2021", "3) 2022", "4) 2023"))
      )
    
  }
}
