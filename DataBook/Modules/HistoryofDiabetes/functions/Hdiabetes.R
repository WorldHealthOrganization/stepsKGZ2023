################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hdiabetes <- function(.data) {
  
  hdiabetes_names <- c("sex", "h6", "h7a", "h7b", "h8", "h9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetes_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetes_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        h6cln = if_else(h6==1 | h6==2, 1, 2, missing = 2),
        h7acln = if_else(h6==1 & (h7a==1 | h7a==2), 1, 2, missing = 2),
        h7acln = replace(h7acln, h6==2 & (h7a==2 | is.na(h7a)), 1),
        h7bcln = if_else(h7a==1 & (h7b==1 | h7b==2), 1, 2, missing = 2),
        h7bcln = replace(h7bcln, h7a==2 & (h7b==2 | is.na(h7b)), 1),
        h7bcln = replace(h7bcln, h6==2 & (h7b==2 | is.na(h7b)), 1),
        cln = if_else(h6cln==1 & h7acln==1 & h7bcln==1 & valid==1, 1, 2, missing = 2),
        c = factor(
          case_when(
            h7b==1 ~ "4) diagnosed within past 12 months",
            h7a==1 & h7b==2 ~ "3) diagnosed, but not within past 12 months",
            h7a==2 ~ "2) measured, not diagnosed", 
            h6==2 ~ "1) never measured",
            TRUE ~ NA_character_
          ),
          levels = c("1) never measured", "2) measured, not diagnosed",  
                     "3) diagnosed, but not within past 12 months",
                     "4) diagnosed within past 12 months")
        ),
        h8cln = if_else(h6==1 & h7a==1 & (h8==1 | h8==2) & valid==1, 1, 2, missing = 2),
        h9cln = if_else(h6==1 & h7a==1 & (h9==1 | h9==2) & valid==1, 1, 2, missing = 2),
        d = factor(ifelse(h8==1, "1) taking meds", 
                          ifelse(h8==2, "2) not taking meds", NA)),
                   levels = c("1) taking meds", "2) not taking meds")),
        e = factor(ifelse(h9==1, "1) taking insulin", 
                          ifelse(h9==2, "2) not taking insulin", NA)),
                   levels = c("1) taking insulin", "2) not taking insulin")),
        # update 2023-10-17: added for question h7a only as ever diagnosed
        h7acln2 = if_else(h6==1 & (h7a==1 | h7a==2), 1, 2, missing = 2),
        f = factor(ifelse(h7a==1, "1) was told",
                          ifelse(h7a==2, "2) was not told", NA)),
                   levels = c("1) was told", "2) was not told")),
        # update 2023-11-21: 
        # added for calculating diabetics (diagnosed/history of diabetes) taking aspirin and statins
        h18cln1 = if_else(h6==1 & h7a==1 & (h18==1 | h18==2) & valid==1, 1, 2, missing = 2),
        g = factor(ifelse(h18==1, "1) taking aspirin", 
                          ifelse(h18==2, "2) not taking aspirin", NA)),
                   levels = c("1) taking aspirin", "2) not taking aspirin")),
        h19cln1 = if_else(h6==1 & h7a==1 & (h19==1 | h19==2) & valid==1, 1, 2, missing = 2),
        h = factor(ifelse(h19==1, "1) taking statins", 
                          ifelse(h19==2, "2) not taking statins", NA)),
                   levels = c("1) taking statins", "2) not taking statins")),
        # added for calculating diabetics (diagnosed/history or blood glucose >=7.0 or took meds today) taking aspirin and statins
        h18cln2 = if_else((b5>=7.0 | b6==1 | (h6==1 & h7a==1)) & (h18==1 | h18==2) & valid==1, 1, 2, missing = 2),
        i = factor(ifelse(h18==1, "1) taking aspirin", 
                          ifelse(h18==2, "2) not taking aspirin", NA)),
                   levels = c("1) taking aspirin", "2) not taking aspirin")),
        h19cln2 = if_else((b5>=7.0 | b6==1 | (h6==1 & h7a==1)) & (h19==1 | h19==2) & valid==1, 1, 2, missing = 2),
        j = factor(ifelse(h19==1, "1) taking statins", 
                          ifelse(h19==2, "2) not taking statins", NA)),
                   levels = c("1) taking statins", "2) not taking statins"))
      )
  }
  
}
