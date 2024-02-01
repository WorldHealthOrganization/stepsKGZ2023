################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

bhdlipids <- function(.data) {
  
  # variable names that are used in the function
  bhdlipids_names <- c("sex", "b17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, bhdlipids_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(bhdlipids_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(b17>=0.1 & b17<=5 & valid==1, 1, 2, missing = 2),
        c = factor(ifelse(b17<1.03, "HDL <1.03mmol/L", "HDL >=1.03 mmol/L"),
                   levels = c("HDL <1.03mmol/L", "HDL >=1.03 mmol/L")),
        d = factor(ifelse(b17<1.29, "HDL <1.29mmol/L", "HDL >=1.29 mmol/L"),
                   levels = c("HDL <1.29mmol/L", "HDL >=1.29 mmol/L")),
        b17mg = b17*38.67
      )
  }
  
}
