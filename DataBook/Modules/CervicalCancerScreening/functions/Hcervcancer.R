################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# Variables used in Hcervcancer
# Age AgeRange Sex Valid CX1 Stratum PSU WStep1 agerange2 UR Region

# ORIGINAL
# hcervcancer <- function(.data) {
# 
#   # variable names that are used in the function
#   hcervcancer_names <- c("sex", "cx1")
#   
#   # check which names are not in the data before proceeding
#   if(!all(i <- rlang::has_name(.data, hcervcancer_names))) {
#     stop(sprintf(
#       "%s doesn't contain: %s",
#       deparse(substitute(.data)),
#       paste(hcervcancer_names[!i], collapse=", ")))
#   } else {
#     .data %>%
#       mutate(cln = if_else((cx1==1 | cx1==2) & valid==1, 1, 2, missing = 2)) %>% 
#       mutate(c = ifelse(cx1==1, "1) has been screened", NA)) %>% 
#       mutate(c = replace(c, cx1==2, "2) has not been screened")) %>% 
#       mutate(c = factor(c))
#   }
# 
# }

# UPDATED 2024-01-13
hcervcancer <- function(.data) {
  
  # variable names that are used in the function
  hcervcancer_names <- c("sex", "cx1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcervcancer_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcervcancer_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else((cx1==1 | cx1==2) & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(cx1==1, "1) has been screened", 
                          ifelse(cx1==2, "2) has not been screened", NA)),
                   levels = c("1) has been screened", "2) has not been screened"))
      )
  }
  
}
