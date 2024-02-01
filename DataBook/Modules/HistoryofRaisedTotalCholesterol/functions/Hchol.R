################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hchol <- function(.data) {
  
  hchol_names <- c("sex", "h12", "h13a", "h13b", "h14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hchol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hchol_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        h12cln = if_else(h12==1 | h12==2, 1, 2, missing = 2),
        h13acln = if_else(h12==1 & (h13a==1 | h13a==2), 1, 2, missing = 2),
        h13acln = replace(h13acln, h12==2 & (h13a==2 | is.na(h13a)), 1),
        h13bcln = if_else(h13a==1 & (h13b==1 | h13b==2), 1, 2, missing = 2),
        h13bcln = replace(h13bcln, h13a==2 & (h13b==2 | is.na(h13b)), 1),
        h13bcln = replace(h13bcln, h12==2 & (h13b==2 | is.na(h13b)), 1),
        cln = if_else(h12cln==1 & h13acln==1 & h13bcln==1 & valid==1, 1, 2, missing = 2),
        h14cln = if_else(h12==1 & h13a==1 & (h14==1 | h14==2) & valid==1, 1, 2, missing = 2),
        c = factor(
          case_when(
            h13b==1 ~ "4) diagnosed within past 12 months",
            h13a==1 & h13b==2 ~ "3) diagnosed, but not within past 12 months", 
            h13a==2 ~ "2) measured, not diagnosed",
            h12==2 ~ "1) never measured",
            TRUE ~ NA_character_
          ), 
          levels = c("1) never measured", "2) measured, not diagnosed",
                     "3) diagnosed, but not within past 12 months",
                     "4) diagnosed within past 12 months")
        ),
        d = factor(ifelse(h14==1, "1) taking meds", 
                          ifelse(h14==2, "2) not taking meds", NA)),
                   levels = c("1) taking meds", "2) not taking meds")),
        # update 2023-10-17: added for question h13a only as ever diagnosed
        h13acln2 = if_else(h12==1 & (h13a==1 | h13a==2), 1, 2, missing = 2),
        e = factor(ifelse(h13a==1, "1) was told",
                          ifelse(h13a==2, "2) was not told", NA)),
                   levels = c("1) was told", "2) was not told"))
      )
  }
  
}
