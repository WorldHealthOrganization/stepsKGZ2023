################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mbmiclass <- function(.data) {
  
  # variable names that are used in the function
  mbmiclass_names <- c("sex", "m8", "m11", "m12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mbmiclass_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mbmiclass_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        bmi = (m12/(m11*m11))*10000,
        m11cln = if_else(m11>=100 & m11<=270 & valid==1, 1, 2, missing = 2),
        m12cln = if_else(m12>=20 & m12<=350 & valid==1, 1, 2, missing = 2),
        bmicln = if_else(m12cln==1 & m11cln==1, 1, 2, missing = 2),
        bmicln = replace(bmicln, (sex=="Women" & m8==1) | (bmi<14 | bmi>60), 2),
        c = ifelse(bmi<18.5, "1) Underweight <18.5", NA),
        c = replace(c, bmi>=18.5 & bmi<25, "2) Normal weight 18.5-24.9"),
        c = replace(c, bmi>=25 & bmi<30, "3) BMI 25.0-29.9"),
        c = replace(c, bmi>=30, "4) Obese >=30"),
        c = factor(c, levels = c("1) Underweight <18.5", "2) Normal weight 18.5-24.9", 
                                 "3) BMI 25.0-29.9", "4) Obese >=30")),
        d = ifelse(bmi<18.5, "1) Underweight <18.5", NA),
        d = replace(d, bmi>=18.5 & bmi<25, "2) Normal weight 18.5-24.9"),
        d = replace(d, bmi>=25, "3) Overweight >=25"),
        d = factor(d, levels = c("1) Underweight <18.5", "2) Normal weight 18.5-24.9", "3) Overweight >=25")),
        c = replace(c, age<19, "2) Normal weight 18.5-24.9"),
        d = replace(d, age<19, "2) Normal weight 18.5-24.9"),
        c = replace(c, sex=="Men" & age==15 & bmi<16.3, "1) Underweight <18.5"),
        d = replace(d, sex=="Men" & age==15 & bmi<16.3, "1) Underweight <18.5"),
        c = replace(c, sex=="Men" & age==16 & bmi<16.7, "1) Underweight <18.5"),
        d = replace(d, sex=="Men" & age==16 & bmi<16.7, "1) Underweight <18.5"),
        c = replace(c, sex=="Men" & age==17 & bmi<17.1, "1) Underweight <18.5"),
        d = replace(d, sex=="Men" & age==17 & bmi<17.1, "1) Underweight <18.5"),
        c = replace(c, sex=="Men" & age==18 & bmi<17.4, "1) Underweight <18.5"),
        d = replace(d, sex=="Men" & age==18 & bmi<17.4, "1) Underweight <18.5"),
        c = replace(c, sex=="Women" & age==15 & bmi<16, "1) Underweight <18.5"),
        d = replace(d, sex=="Women" & age==15 & bmi<16, "1) Underweight <18.5"),
        c = replace(c, sex=="Women" & age==16 & bmi<16.3, "1) Underweight <18.5"),
        d = replace(d, sex=="Women" & age==16 & bmi<16.3, "1) Underweight <18.5"),
        c = replace(c, sex=="Women" & age==17 & bmi<16.4, "1) Underweight <18.5"),
        d = replace(d, sex=="Women" & age==17 & bmi<16.4, "1) Underweight <18.5"),
        c = replace(c, sex=="Women" & age==18 & bmi<16.5, "1) Underweight <18.5"),
        d = replace(d, sex=="Women" & age==18 & bmi<16.5, "1) Underweight <18.5"),
        c = replace(c, sex=="Men" & age==15 & bmi>23.1, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Men" & age==15 & bmi>23.1, "3) Overweight >=25"),
        c = replace(c, sex=="Men" & age==16 & bmi>23.9, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Men" & age==16 & bmi>23.9, "3) Overweight >=25"),
        c = replace(c, sex=="Men" & age==17 & bmi>24.6, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Men" & age==17 & bmi>24.6, "3) Overweight >=25"),
        c = replace(c, sex=="Men" & age==18 & bmi>25.2, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Men" & age==18 & bmi>25.2, "3) Overweight >=25"),
        c = replace(c, sex=="Women" & age==15 & bmi>23.8, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Women" & age==15 & bmi>23.8, "3) Overweight >=25"),
        c = replace(c, sex=="Women" & age==16 & bmi>24.3, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Women" & age==16 & bmi>24.3, "3) Overweight >=25"),
        c = replace(c, sex=="Women" & age==17 & bmi>24.6, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Women" & age==17 & bmi>24.6, "3) Overweight >=25"),
        c = replace(c, sex=="Women" & age==18 & bmi>24.9, "3) BMI 25.0-29.9"),
        d = replace(d, sex=="Women" & age==18 & bmi>24.9, "3) Overweight >=25"),
        c = replace(c, sex=="Men" & age==15 & bmi>27.4, "4) Obese >=30"),
        c = replace(c, sex=="Men" & age==16 & bmi>28.3, "4) Obese >=30"),
        c = replace(c, sex=="Men" & age==17 & bmi>29, "4) Obese >=30"),
        c = replace(c, sex=="Men" & age==18 & bmi>29.5, "4) Obese >=30"),
        c = replace(c, sex=="Women" & age==15 & bmi>28.6, "4) Obese >=30"),
        c = replace(c, sex=="Women" & age==16 & bmi>29.1, "4) Obese >=30"),
        c = replace(c, sex=="Women" & age==17 & bmi>29.4, "4) Obese >=30"),
        c = replace(c, sex=="Women" & age==18 & bmi>29.6, "4) Obese >=30")
      )
  }
  
}
