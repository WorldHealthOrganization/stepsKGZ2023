################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hdiabetestrad <- function(.data) {
  
  hdiabetestrad_names <- c("sex", "h6", "h7a", "h10", "h11")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetestrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetestrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        h10cln = if_else(h6==1 & h7a==1 & (h10==1 | h10==2) & valid==1, 1, 2, missing = 2),
        h11cln = if_else(h6==1 & h7a==1 & (h11==1 | h11==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(h10==1, "1) has seen a traditional healer",
                          ifelse(h10==2, "2) has not seen a traditional healer", NA)),
                   levels = c("1) has seen a traditional healer", 
                              "2) has not seen a traditional healer")),
        d = factor(ifelse(h11==1, "1) taking herbal/traditional remedy", 
                          ifelse(h11==2, "2) not taking herbal/traditional remedy", NA)),
                   levels = c("1) taking herbal/traditional remedy",
                              "2) not taking herbal/traditional remedy"))
      )
  }
  
}
