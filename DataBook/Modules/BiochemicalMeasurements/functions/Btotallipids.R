################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

btotallipids <- function(.data) {
  
  # variable names that are used in the function
  btotallipids_names <- c("sex", "b8", "b9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, btotallipids_names))) {
    stop(sprintf(
      "%s doesn't contain: %s", 
      deparse(substitute(.data)),
      paste(btotallipids_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        cln = if_else(b8>=2 & b8<=12 & valid==1, 1, 2, missing = 2),
        c = ifelse(b8<5.0, "total cholesterol < 5.0", NA),
        d = ifelse(b8<5.0, "total cholesterol < 6.2", NA),
        e = ifelse(b8<5.0, "total cholesterol < 5.0", NA),
        f = ifelse(b8<5.0, "total cholesterol < 6.2", NA),
        c = replace(c, b8>=5.0 & b8<6.2, "total cholesterol >= 5.0 or on meds"),
        d = replace(d, b8>=5.0 & b8<6.2, "total cholesterol < 6.2"),
        e = replace(e, b8>=5.0 & b8<6.2, "total cholesterol >= 5.0"),
        f = replace(f, b8>=5.0 & b8<6.2, "total cholesterol < 6.2"),
        c = replace(c, b8>=6.2, "total cholesterol >= 5.0 or on meds"),
        d = replace(d, b8>=6.2, "total cholesterol >= 6.2 or on meds"),
        e = replace(e, b8>=6.2, "total cholesterol >= 5.0"),
        f = replace(f, b8>=6.2, "total cholesterol >= 6.2"),
        c = replace(c, b9==1, "total cholesterol >= 5.0 or on meds"),
        d = replace(d, b9==1, "total cholesterol >= 6.2 or on meds"),
        b8mg = b8*38.67,
        c = factor(c, levels = c("total cholesterol < 5.0", "total cholesterol >= 5.0 or on meds")),
        d = factor(d, levels = c("total cholesterol < 6.2", "total cholesterol >= 6.2 or on meds")),
        e = factor(e, levels = c("total cholesterol < 5.0", "total cholesterol >= 5.0")),
        f = factor(f, levels = c("total cholesterol < 6.2", "total cholesterol >= 6.2"))
      )
  }
}



