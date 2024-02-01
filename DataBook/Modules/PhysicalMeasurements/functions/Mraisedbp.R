################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mraisedbp <- function(.data) {
  
  # variable names that are used in the function
  mraisedbp_names <- c("sex", "m4a", "m4b", "m5a", "m5b", "m6a", "m6b", "m7", "h1", "h2a", "h3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mraisedbp_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mraisedbp_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        treatment = if_else(h3==1 | m7==1, 1, 2, missing = 2),
        m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2),
        m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2),
        m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2),
        m4bcln = if_else(m4b>=30 & m4b<=200, 1, 2, missing = 2),
        m5bcln = if_else(m5b>=30 & m5b<=200, 1, 2, missing = 2), 
        m6bcln = if_else(m6b>=30 & m6b<=200, 1, 2, missing = 2),
        ###
        sbp = case_when(
          ((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1) ~ (m5a+m6a)/2,
          (m4acln==1 & m5acln==2 & m6acln==1) ~ (m4a+m6a)/2,
          (m4acln==1 & m5acln==1 & m6acln==2) ~ (m4a+m5a)/2,
          TRUE ~ NA_real_
        ),
        dbp = case_when(
          ((m4bcln==1 | m4bcln==2) & m5bcln==1 & m6bcln==1) ~ (m5b+m6b)/2,
          (m4bcln==1 & m5bcln==2 & m6bcln==1) ~ (m4b+m6b)/2,
          (m4bcln==1 & m5bcln==1 & m6bcln==2) ~ (m4b+m5b)/2,
          TRUE ~ NA_real_
        ),
        sbpcln = if_else(sbp>=40 & sbp<=300 & valid==1, 1, 2, missing = 2),  
        dbpcln = if_else(dbp>=30 & dbp<=200 & valid==1, 1, 2, missing = 2),
        cln = if_else(sbpcln==1 & dbpcln==1 & valid==1, 1, 2),
        cln = replace(cln, (m7==1 | h3==1) & (h2a==2 | is.na(h2a)), 2),
        ###
        clnnomeds = if_else(cln==1 & treatment==2, 1, 2, missing = 2),
        raisedsbp = case_when(
          sbp<140 ~ 1,
          sbp>=140 & sbp<160 ~ 2,
          sbp>=160 ~ 3,
          TRUE ~ NA_real_
        ),
        raiseddbp = case_when(
          dbp<90 ~ 1,
          dbp>=90 & dbp<100 ~ 2,
          dbp>=100 ~ 3,
          TRUE ~ NA_real_
        ),
        raisedbp_140_90 = factor(if_else((raisedsbp>=2 | raiseddbp>=2), "1)SBP>=140 and/or DBP>=90",
                                         "2)SBP<140 and DBP<90", missing = "2)SBP<140 and DBP<90"),
                                 levels = c("1)SBP>=140 and/or DBP>=90", "2)SBP<140 and DBP<90")),
        raisedbp_160_100 = factor(if_else((raisedsbp==3 | raiseddbp==3), "1)SBP>=160 and/or DBP>=100",
                                          "2)SBP<160 and DBP<100", missing = "2)SBP<160 and DBP<100"),
                                  levels = c("1)SBP>=160 and/or DBP>=100", "2)SBP<160 and DBP<100")),
        raisedbp_140_90_or_meds = factor(if_else(raisedsbp>=2 | raiseddbp>=2 | treatment==1,
                                                 "1)SBP>=140 and/or DBP>=90 or on meds", 
                                                 "2)SBP<140 and DBP<90 and not on meds",
                                                 missing = "2)SBP<140 and DBP<90 and not on meds"),
                                         levels = c("1)SBP>=140 and/or DBP>=90 or on meds", 
                                                    "2)SBP<140 and DBP<90 and not on meds")),
        raisedbp_160_100_or_meds = factor(if_else(raisedsbp==3 | raiseddbp==3 | treatment==1,
                                                  "1)SBP>=160 and/or DBP>=100 or on meds",
                                                  "2)SBP<160 and DBP<100 and not on meds",
                                                  missing = "2)SBP<160 and DBP<100 and not on meds"),
                                          levels = c("1)SBP>=160 and/or DBP>=100 or on meds",
                                                     "2)SBP<160 and DBP<100 and not on meds")),
        ###
        htn_control_cln = if_else(cln==1 & raisedbp_140_90_or_meds=="1)SBP>=140 and/or DBP>=90 or on meds",
                                  1, 2, missing = 2),
        htn_control = factor(case_when(
          htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & (h1==2 | h2a==2) ~ "1) Not previously diagnosed",
          htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & h2a==1 & treatment==2 ~ "2) Previously diagnosed, not on meds",
          htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & h2a==1 & treatment==1 ~ "3) Previously diagnosed, on meds, not controlled",
          htn_control_cln==1 & raisedsbp==1 & raiseddbp==1 & h2a==1 & treatment==1 ~ "4) Previously diagnosed, on meds, controlled",
          TRUE ~ NA_character_
        ),
        levels = c("1) Not previously diagnosed", "2) Previously diagnosed, not on meds", 
                   "3) Previously diagnosed, on meds, not controlled", "4) Previously diagnosed, on meds, controlled")),
        ###
        bp_control_old = factor(case_when(
          raisedsbp==1 & raiseddbp==1 & treatment==1 ~ "1) on meds and BP not raised",
          (raisedsbp==2 | raisedsbp==3 | raiseddbp==2 | raiseddbp==3) & treatment==1 ~ "2) on meds and BP raised",
          (raisedsbp==2 | raisedsbp==3 | raiseddbp==2 | raiseddbp==3) & treatment==2 ~ "3) not on meds and BP raised",
          TRUE ~ NA_character_
        ),
        levels = c("1) on meds and BP not raised", "2) on meds and BP raised", "3) not on meds and BP raised"))
        
      )
  }
  
}
