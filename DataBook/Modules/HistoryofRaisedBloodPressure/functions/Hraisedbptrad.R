################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hraisedbptrad <- function(.data) {
  
  hraisedbptrad_names <- c("sex", "h1", "h2a", "h4", "h5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hraisedbptrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hraisedbptrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        cln = if_else(h1==1 & h2a==1 & valid==1, 1, 2, missing = 2),
        h4cln = if_else(cln==1 & h4==1 | h4==2, 1, 2, missing = 2),
        h5cln = if_else(cln==1 & h5==1 | h5==2, 1, 2, missing = 2),
        c = factor(ifelse(h4==1, "1) has seen a traditional healer", 
                          ifelse(h4==2, "2) has not seen a traditional healer", NA)),
                   levels = c("1) has seen a traditional healer",
                              "2) has not seen a traditional healer")),
        d = factor(ifelse(h5==1, "1) taking herbal/traditional remedy", 
                          ifelse(h5==2, "2) not taking herbal/traditional remedy", NA)),
                   levels = c("1) taking herbal/traditional remedy",
                              "2) not taking herbal/traditional remedy"))
      )
  }
  
}

