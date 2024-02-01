################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

aunrecordedpastweek <- function(.data) {
  
  aunrecordedpastweek_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", 
                                 "a10d", "a10e", "a10f", "a10g", "a11", "a12a", 
                                 "a12b", "a12c", "a12d", "a12e")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aunrecordedpastweek_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aunrecordedpastweek_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        allmissing = 2,
        across(c(a10a, a10b, a10c, a10d, a10e, a10f, a10g), ~na_if(., 77)),
        allmissing = replace(allmissing, is.na(a10a) & is.na(a10b) & is.na(a10c) &
                               is.na(a10d) & is.na(a10e) & is.na(a10f) & is.na(a10g), 1),
        a10a = replace(a10a, allmissing==2 & is.na(a10a), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10b), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10c), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10d), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10e), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10f), 0),
        a10a = replace(a10a, allmissing==2 & is.na(a10g), 0),
        a10cln = if_else(a10a<51 & a10b<51 & a10c<51 & a10d<51 & a10e<51 & a10f<51 & a10g<51, 1, 2),
        past7total = a10a + a10b + a10c + a10d + a10e + a10f + a10g,
        cln = if_else(a1==1 & a2==1 & a5==1 & a10cln==1 & valid==1 & past7total>0, 1, 2, missing = 2),
        across(c(a12a, a12b, a12c, a12d, a12e), ~na_if(., 77)),
        across(c(a12a, a12b, a12c, a12d, a12e), ~replace(., (a11==1 & is.na(.)) | a11==2, 0)),
        past7unrec = a12a + a12b + a12c + a12d + a12e,
        cln = replace(cln, past7unrec>past7total & !is.na(past7unrec), 2),
        past7totalwt = past7total*wstep1,
        past7unrecwt = past7unrec*wstep1,
        percentunrec = past7unrecwt/past7totalwt,
        percentunrec2 = past7unrec/past7total
      ) %>% 
      filter(cln == 1)
  }
  
}
