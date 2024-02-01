################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

cvdrisk <- function(.data) {
  
  # variable names that are used in the function
  cvdrisk_names <- c("sex", "c1", "h6", "h7a", "h17", "m4a", "m5a", "m6a", "t1", 
                     "t8", "h3", "h8", "h9", "h14", "h18", "h19", "h20a", "h20b", 
                     "h20c", "h20d", "h20e", "h20f", "b1", "b5", "b6", "b8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cvdrisk_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cvdrisk_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t10 = NA,
        t11 = 10,
        t11type = "years",
        smokecln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        smokecln = replace(smokecln, t1==2 & is.na(t8), 2),
        smoker = if_else(t1==1, 1, 2, missing = 2),
        # Calculate T11b (months) AND T11c (weeks) into years
        t11a = ifelse(t11type>="years", t11, NA),
        t11byrs = ifelse(t11type>="months", t11/12, NA),
        t11cyrs = ifelse(t11type>="weeks", t11/52, NA),
        t10cln = if_else(t10>=7 & t10<=69, 1, 2, missing = 2),
        t11acln = if_else(t11a>0 & t11a<=61, 1, 2, missing = 2),
        t11bcln = if_else(is.na(t11byrs), 2, 1, missing = 1),
        t11ccln = if_else(is.na(t11cyrs), 2, 1, missing = 1),
        # Order the priority of responses so that T10 takes precedence over other responses
        stop = ifelse(t11ccln==1, t11cyrs, NA),
        stop = replace(stop, t11bcln==1, t11byrs),
        stop = replace(stop, t11acln==1, t11a),
        stop = replace(stop, t10cln==1, age-t10),
        smokecln = replace(smokecln, t10cln==2 & t11acln==2 & t11bcln==2 & t11ccln==2 & t1==2 & t8==1, 2),
        smokecln = replace(smokecln, t1==2 & t8==1 & (is.na(stop) | stop>61 | stop<0), 2),
        smoker = replace(smoker, stop<=1, 1),
        m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2),
        m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2),
        m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2),
        sbp = ifelse((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1, (m5a + m6a)/2, NA),
        sbp = ifelse(m4acln==1 & m5acln==2 & m6acln==1, (m4a + m6a)/2, sbp),
        sbp = ifelse(m4acln==1 & m5acln==1 & m6acln==2, (m4a + m5a)/2, sbp),
        sbpcln = if_else((sbp<40 | sbp>300) | is.na(sbp), 2, 1, missing = 1),
        chol = ifelse(b8>=2 & b8<=12, b8, NA),
        cholcln = if_else(b8>=2 & b8<=12, 1, 2, missing = 2),
        diabcln = if_else(b5>=1 & b5<=35 & b1==2, 1, 2, missing = 2),
        diab = if_else(b5>=6.1 | b6==1, 1, 2, missing = 2),
        diabcln = replace(diabcln, h6==1 & h7a==1, 1),
        diab = replace(diab, h6==1 & h7a==1, 1),
        cln = if_else(diabcln==1 & cholcln==1 & sbpcln==1 & smokecln==1 & valid==1, 1, 2, missing = 2),
        highrisk = 2,
        # MALES WITH DIABETES
        highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=6 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=8 & sbp>=120) | (chol>=7 & sbp>=140)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=5 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=6 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==2 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        # FEMALES WITH DIABETES
        highrisk = replace(highrisk, c1==2 & (age>=40 & age<60) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==2 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=6 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==2 & (age>=40 & age<60) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==2 & (age>=60 & age<70) & smoker==2 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        # MEN WITHOUT DIABETES
        highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=6 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==2 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==1 & (age>=40 & age<60) & smoker==2 & diab==2 & (chol>=7 & sbp>=180), 1),
        highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==2 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1),
        # WOMEN WITHOUT DIABETES
        highrisk = replace(highrisk, c1==2 & (age>=40 & age<70) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1),
        highrisk = replace(highrisk, c1==2 & (age>=40 & age<70) & smoker==2 & diab==2 & (chol>=7 & sbp>=180), 1),
        highrisk = replace(highrisk, h17==1, 1),
        ###
        c = factor(ifelse(highrisk==1, "risk 30% or more or has CVD", 
                          ifelse(highrisk==2, "risk less than 30%", NA)),
                   levels = c("risk 30% or more or has CVD", "risk less than 30%")),
        drugs = if_else(h3==1 | h8==1 | h9==1 | h14==1 | h18==1 | h19==1, 1, 2, missing = 2),
        counseling = if_else(h20a==1 | h20b==1 | h20c==1 | h20d==1 | h20e==1 | h20f==1, 1, 2, missing = 2),
        d = factor(if_else(drugs==1 & counseling==1, "1) received drug therapy and counseling", 
                           "2) did not receive drug therapy and counseling", missing = "2) did not receive drug therapy and counseling"),
                   levels = c("1) received drug therapy and counseling", "2) did not receive drug therapy and counseling")),
        agerangecvd = factor(ifelse(age>=40 & age<=54, "40–54", 
                                    ifelse(age>=55 & age<=69, "55–69", NA)),
                             levels = c("40–54", "55–69")),
        cln = replace(cln, age<40, 2)
      )
  }
}

