################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tpcigpromos <- function(.data) {
  
  # variable names that are used in the function
  tpcigpromos_names <- c("sex", "tp3a", "tp3b", "tp3c", "tp3d", "tp3e", "tp3f")
  
  # check which names are not in the data before proceeding 
  if(!all(i <- rlang::has_name(.data, tpcigpromos_names))) {
    stop(sprintf(
      "%s doesn't contain: %s", 
      deparse(substitute(.data)),
      paste(tpcigpromos_names[!i], collapse=", ")
    ))
  } else {
    .data %>%
      mutate(
        a = factor(ifelse(tp3a==1, "1) noticed free samples", 
                          ifelse(tp3a==2, "2) did not notice free samples", NA)),
                   levels = c("1) noticed free samples", "2) did not notice free samples")),
        b = factor(ifelse(tp3b==1, "1) noticed cigs on sale", 
                          ifelse(tp3b==2, "2) did not notice cigs on sale", NA)),
                   levels = c("1) noticed cigs on sale", "2) did not notice cigs on sale")),
        c = factor(ifelse(tp3c==1, "1) noticed cig coupons", 
                          ifelse(tp3c==2, "2) did not notice cig coupons", NA)),
                   levels = c("1) noticed cig coupons", "2) did not notice cig coupons")),
        d = factor(ifelse(tp3d==1, "1) noticed free gifts", 
                          ifelse(tp3d==2, "2) did not notice free gifts", NA)),
                   levels = c("1) noticed free gifts", "2) did not notice free gifts")),
        e = factor(ifelse(tp3e==1, "1) noticed branded clothing", 
                          ifelse(tp3e==2, "2) did not notice branded clothing", NA)),
                   levels = c("1) noticed branded clothing", "2) did not notice branded clothing")),
        f = factor(ifelse(tp3f==1, "1) noticed mail promos", 
                          ifelse(tp3f==2, "2) did not notice mail promos", NA)),
                   levels = c("1) noticed mail promos", "2) did not notice mail promos")),
        tp3acln = if_else(valid==1, 1, 2, missing = 2),
        tp3acln = replace(tp3acln, is.na(a), 2),
        tp3bcln = if_else(valid==1, 1, 2, missing = 2),
        tp3bcln = replace(tp3bcln, is.na(b), 2),
        tp3ccln = if_else(valid==1, 1, 2, missing = 2),
        tp3ccln = replace(tp3ccln, is.na(c), 2),
        tp3dcln = if_else(valid==1, 1, 2, missing = 2),
        tp3dcln = replace(tp3dcln, is.na(d), 2),
        tp3ecln = if_else(valid==1, 1, 2, missing = 2),
        tp3ecln = replace(tp3ecln, is.na(e), 2),
        tp3fcln = if_else(valid==1, 1, 2, missing = 2),
        tp3fcln = replace(tp3fcln, is.na(f), 2),
        allcln = if_else(tp3acln==1 & tp3bcln==1 & tp3ccln==1 & 
                           tp3dcln==1 & tp3ecln==1 & tp3fcln==1, 1, 2, missing = 2),
        g = factor(if_else(tp3a==1 | tp3b==1 | tp3c==1 | tp3d==1 | tp3e==1 | tp3f==1, 
                           "1) noticed any promotion", "2) did not notice any promotion", 
                           missing = "2) did not notice any promotion"),
                   levels = c("1) noticed any promotion", "2) did not notice any promotion"))
      )
  }
}
