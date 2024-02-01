################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hcholtrad <- function(.data) {
  
  hcholtrad_names <- c("sex", "h12", "h13a", "h13b", "h14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcholtrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcholtrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(h12==1 & h13a==1 & valid==1, 1, 2, missing = 2),
        h15cln = if_else(cln==1 & h15==1 | h15==2, 1, 2, missing = 2),
        h16cln = if_else(cln==1 & h16==1 | h16==2, 1, 2, missing = 2),
        c = factor(ifelse(h15==1, "1) has seen a traditional healer", 
                          ifelse(h15==2, "2) has not seen a traditional healer", NA)),
                   levels = c("1) has seen a traditional healer",
                              "2) has not seen a traditional healer")),
        d = factor(ifelse(h16==1, "1) taking herbal/traditional remedy", 
                          ifelse(h16==2, "2) not taking herbal/traditional remedy", NA)),
                   levels = c("1) taking herbal/traditional remedy",
                              "2) not taking herbal/traditional remedy"))
      )
  }
  
}
