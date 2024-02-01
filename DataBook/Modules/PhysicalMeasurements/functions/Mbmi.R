################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mbmi <- function(.data) {
  
  # variable names that are used in the function
  mbmi_names <- c("sex", "m8", "m11", "m12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mbmi_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mbmi_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        bmi = (m12/(m11*m11))*10000,
        m11cln = if_else(m11>=100 & m11<=270 & valid==1, 1, 2, missing = 2),
        m12cln = if_else(m12>=20 & m12<=350 & valid==1, 1, 2, missing = 2),
        bmicln = if_else(m12cln==1 & m11cln==1, 1, 2, missing = 2),
        bmicln = replace(bmicln, (sex=="Women" & m8==1) | (bmi<14 | bmi>60), 2),
        m11cln = replace(m11cln, sex=="Women" & m8==1, 2),
        m12cln = replace(m12cln, sex=="Women" & m8==1, 2)
      )
  }
  
}
