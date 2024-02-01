################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

whs_depression <- function(.data) {
  
  whs_depression_names <- c("sex","da1","da5","da6","da7","da8","da9",
                            "da10","da11","da12","da13","da14","da15","da16",
                            "da17","da18","da19","da20","da21","da22","da23")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, whs_depression_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(whs_depression_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        # WHS CODE (1)
        across(c(da1, da2, da3, da4, da5, da6, da7, da8, da9, da10, da11, da12, da13, da14, 
                 da15, da16, da17, da18, da19, da20, da21, da22, da23), ~replace(., . >2, NA)),
        # WHS CODE (2)
        across(c(da9, da10, da11, da12, da13, da14, da15, da16, da17,
                 da18, da19, da20, da21, da22, da23), ~replace(., da6==2 & da7==2 & da8==2, 2)),
        # WHS CODE (3)
        bb1 = if_else(da6==1 & da9==1 & da10==1, 1, 0, missing = 0),
        bb2 = if_else(da7==1 | da21==1, 1, 0, missing = 0),
        bb3 = if_else(da8==1, 1, 0, missing = 0),
        cc1 = if_else(da19==1 | da20==1, 1, 0, missing = 0),
        cc2 = if_else(da17==1 | da18==1, 1, 0, missing = 0),
        cc3 = if_else(da22==1 | da23==1, 1, 0, missing = 0),
        cc4 = if_else(da12==1 | da15==1, 1, 0, missing = 0),
        cc5 = if_else(da16==1, 1, 0, missing = 0),
        cc6 = if_else(da13==1 | da14==1, 1, 0, missing = 0),
        cc7 = if_else(da11==1, 1, 0, missing = 0),
        bb = bb1+bb2+bb3,
        bbc = bb1+bb2+bb3+cc1+cc2+cc3+cc4+cc5+cc6+cc7,
        depression = 0,
        depression = replace(depression, is.na(da6) | is.na(da7) | is.na(da8), NA),
        depression = replace(depression, bb>=2 & bbc>=4, 1),
        depression = replace(depression, da1==1 & da5==1, 1),
        # NOTE: Without filtering NA values in depression, a cln variable is needed to 
        # get the same results as in STATA
        cln = if_else(depression==1 | depression==0, 1, 2, missing = 2),
        ## Label values depression yes/no for categorization 
        c = factor(ifelse(depression==1, "Yes", 
                          ifelse(depression==0, "No", NA)), 
                   levels = c("Yes", "No")),
        ### Additional analysis 
        ### Prepare data by creating CLN variables for DA1, DA4, DA5
        ### "Told by a doctor or health worker that have depression"
        ### DA1. Have you ever been told by a doctor or health care professional that you have depression?
        da1cln = if_else(da1==1 | da1==2, 1, 2, missing = 2),
        d = factor(ifelse(da1==1, "1) told", 
                          ifelse(da1==2, "2) wasn't told", NA)),
                   levels = c("1) told", "2) wasn't told")),
        ### "Have been taking medications or other treatment for depression"
        ### DA4. Meds or treatment in the last 12 months
        da4cln = if_else(da1==1 & (da4==1 | da4==2), 1, 2, missing = 2),
        e = factor(ifelse(da4==1, "1) on medication", 
                          ifelse(da4==2, "2) not on medication", NA)),
                   levels = c("1) on medication", "2) not on medication")),
        ### DA5. Meds or treatment in the last 2 weeks
        da5cln = if_else(da4==1 & (da5==1 | da5==2), 1, 2, missing = 2),
        f = factor(ifelse(da5==1, "1) on medication", 
                          ifelse(da5==2, "2) not on medication", NA)),
                   levels = c("1) on medication", "2) not on medication")),
        ### DA22. Did you think of death, or wish you were dead?
        da22cln = if_else((da6==1 | da7==1 | da8==1) & (da22==1 | da22==2), 1, 2, missing = 2),
        g = factor(ifelse(da22==1, "1) thought", 
                          ifelse(da22==2, "2) didn't think", NA)),
                   levels = c("1) thought", "2) didn't think")),
        ### DA23. During this period, did you ever try to end your life?
        da23cln = if_else((da6==1 | da7==1 | da8==1) & (da23==1 | da23==2), 1, 2, missing = 2),
        h = factor(ifelse(da23==1, "1) ever tried", 
                          ifelse(da23==2, "2) never tried", NA)),
                   levels = c("1) ever tried", "2) never tried"))
      )
  }
  
}

