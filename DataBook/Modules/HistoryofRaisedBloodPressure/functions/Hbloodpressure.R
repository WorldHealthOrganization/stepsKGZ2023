################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hbloodpressure <- function(.data) {
  
  hbloodpressure_names <- c("sex", "h1", "h2a", "h2b", "h3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hbloodpressure_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hbloodpressure_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        h1cln = if_else(h1==1 | h1==2, 1, 2, missing = 2),
        h2acln = if_else(h1==1 & (h2a==1 | h2a==2), 1, 2, missing = 2),
        h2acln = replace(h2acln, h1==2 & (h2a==2 | is.na(h2a)), 1),
        h2bcln = if_else(h2a==1 & (h2b==1 | h2b==2), 1, 2, missing = 2),
        h2bcln = replace(h2bcln, h2a==2 & (h2b==2 | is.na(h2b)), 1),
        h2bcln = replace(h2bcln, h1==2 & (h2b==2 | is.na(h2b)), 1),
        cln = if_else(h1cln==1 & h2acln==1 & h2bcln==1 & valid==1, 1, 2, missing = 2),
        h3cln = if_else(h1==1 & h2a==1 & (h3==1 | h3==2) & valid==1, 1, 2, missing = 2),
        c = factor(
          case_when(
            h2b==1 ~ "4) diagnosed within past 12 months", 
            h2a==1 & h2b==2 ~ "3) diagnosed, but not within past 12 months",
            h2a==2 ~ "2) measured, not diagnosed",
            h1==2 ~ "1) never measured", 
            TRUE ~ NA_character_
          ),
          levels = c("1) never measured", "2) measured, not diagnosed",
                     "3) diagnosed, but not within past 12 months",
                     "4) diagnosed within past 12 months")
        ),
        d = factor(ifelse(h3==1, "1) taking meds",
                          ifelse(h3==2, "2) not taking meds", NA)),
                   levels = c("1) taking meds", "2) not taking meds")),
        # update 2023-10-17: added for question h2a only as ever diagnosed
        h2acln2 = if_else(h1==1 & (h2a==1 | h2a==2), 1, 2, missing = 2),
        e = factor(ifelse(h2a==1, "1) was told",
                          ifelse(h2a==2, "2) was not told", NA)),
                   levels = c("1) was told", "2) was not told"))
      )
  }
  
}
