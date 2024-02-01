################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cworkpaid <- function(.data) {
  
  # variable names used in function
  cworkpaid_names <- c("sex", "c8")
  
  # check required columns
  if(!all(i <- rlang::has_name(.data, cworkpaid_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cworkpaid_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        c = factor(
          case_when(
            c8==1 ~ "1) Government employee",
            c8==2 ~ "2) Non-government employee",
            c8==3 ~ "3) Self-employed",
            c8 %in% 4:9 ~ "4) Unpaid",
            TRUE ~ NA_character_  
          ),
          levels = c("1) Government employee", "2) Non-government employee",
                     "3) Self-employed", "4) Unpaid")
        ),
        cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1)
      )
  }
}
