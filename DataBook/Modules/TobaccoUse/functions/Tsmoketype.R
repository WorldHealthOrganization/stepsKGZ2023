################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmoketype <- function(.data) {
  
  # variable names that are used in the function
  tsmoketype_names <- c("sex", "t1", "t2", "t5a", "t5b", "t5c", "t5d", "t5e", "t5f", "t5g")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmoketype_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmoketype_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(t1==1 & t2==1 & valid==1, 1, 2, missing = 2),
        t5acln = if_else(t5a>=0 & t5a<=50, 1, 2, missing = 2),
        t5bcln = if_else(t5b>=0 & t5b<=50, 1, 2, missing = 2),
        t5ccln = if_else(t5c>=0 & t5c<=50, 1, 2, missing = 2),
        t5dcln = if_else(t5d>=0 & t5d<=50, 1, 2, missing = 2),
        t5ecln = if_else(t5e>=0 & t5e<=50, 1, 2, missing = 2),
        t5fcln = if_else(t5f>=0 & t5f<=50, 1, 2, missing = 2),
        t5gcln = if_else(t5g>=0 & t5g<=50, 1, 2, missing = 2),
        cigs = t5a+t5b,
        cigcln = if_else(t5acln==1 & t5bcln==1 & cigs>0, 1, 2, missing = 2)
      )
  }
  
}
