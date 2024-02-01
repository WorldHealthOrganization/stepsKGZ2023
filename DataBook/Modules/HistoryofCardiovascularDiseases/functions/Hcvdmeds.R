################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hcvdmeds <- function(.data) {
  
  # variable names that are used in the function
  hcvdmeds_names <- c("sex", "h17", "h18", "h19")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcvdmeds_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcvdmeds_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        h18cln = if_else((h18==1 | h18==2) & valid==1, 1, 2, missing = 2),
        h19cln = if_else((h19==1 | h19==2) & valid==1, 1, 2, missing = 2),
        # subset of people with CVD (added to the original script)
        h17cln = if_else(h17==1, 1, 2, missing = 2),
        h18cln1 = if_else(h17cln==1 & (h18==1 | h18==2) & valid==1, 1, 2, missing = 2),
        h19cln1 = if_else(h17cln==1 & (h19==1 | h19==2) & valid==1, 1, 2, missing = 2),
        ###
        c = factor(ifelse(h18==1, "1) taking aspirin", 
                          ifelse(h18==2, "2) not taking aspirin", NA)),
                   levels = c("1) taking aspirin", "2) not taking aspirin")),
        d = factor(ifelse(h19==1, "1) taking statins", 
                          ifelse(h19==2, "2) not taking statins", NA)),
                   levels = c("1) taking statins", "2) not taking statins"))
      )
  }
  
}
