################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

pcomposition <- function(.data) {
  
  # variable names that are used in the function
  pcomposition_names <- c("sex", "p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", 
                          "p7", "p8", "p9a", "p9b", "p10", "p11", "p12a", "p12b", 
                          "p13", "p14", "p15a", "p15b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, pcomposition_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(pcomposition_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        p1t3 = ifelse(p1t3cln==1, p2*p3, NA),
        p4t6 = ifelse(p4t6cln==1, p5*p6, NA),
        p7t9 = ifelse(p7t9cln==1, p8*p9, NA),
        p10t12 = ifelse(p10t12cln==1, p11*p12, NA),
        p13t15 = ifelse(p13t15cln==1, p14*p15, NA),
        ptotal = p1t3+p4t6+p7t9+p10t12+p13t15,
        percentwork = (p1t3+p4t6)/ptotal*100,
        percenttrans = p7t9/ptotal*100,
        percentrec = (p10t12+p13t15)/ptotal*100,
        cln = if_else(p1t3cln==1 & p4t6cln==1 & p7t9cln==1 & p10t12cln==1 & 
                        p13t15cln==1 & valid==1, 1, 2, missing = 2),
        cln = replace(cln, is.na(p1) & is.na(p4) & is.na(p7) & is.na(p10) & is.na(p13), 2),
        cln = replace(cln, ptotal==0, 2)
      )
  }
  
}
