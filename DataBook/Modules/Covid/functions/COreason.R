################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

coreason <- function(.data) {
  
  coreason_names <- c("sex", "co3a", "co3b", "co3c", "co3d", "co3e", "co3f", 
                      "co3g", "co3h", "co3i", "co3j", "co3k", "co3l")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, coreason_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(coreason_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        co3acln = if_else((co3a==1 | co3a==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3bcln = if_else((co3b==1 | co3b==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3ccln = if_else((co3c==1 | co3c==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3dcln = if_else((co3d==1 | co3d==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3ecln = if_else((co3e==1 | co3e==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3fcln = if_else((co3f==1 | co3f==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3gcln = if_else((co3g==1 | co3g==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3hcln = if_else((co3h==1 | co3h==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3icln = if_else((co3i==1 | co3i==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3jcln = if_else((co3j==1 | co3j==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3kcln = if_else((co3k==1 | co3k==2) & co1==2 & valid==1, 1, 2, missing = 2),
        co3lcln = if_else((co3l==1 | co3l==2) & co1==2 & valid==1, 1, 2, missing = 2),
        a = factor(case_when(
          co3a==1 ~ "1) Inconvenient location",
          co3a==2 ~ "2) Convenient location",
          TRUE ~ NA_character_
        ), levels = c("1) Inconvenient location", "2) Convenient location")),
        b = factor(case_when(
          co3b==1 ~ "1) Preferred vaccine unavailable",
          co3b==2 ~ "2) Preferred vaccine available",
          TRUE ~ NA_character_
        ), levels = c("1) Preferred vaccine unavailable", "2) Preferred vaccine available")),
        c = factor(case_when(
          co3c==1 ~ "1) Has a medical condition",
          co3c==2 ~ "2) No medical condition",
          TRUE ~ NA_character_
        ), levels = c("1) Has a medical condition", "2) No medical condition")),
        d = factor(case_when(
          co3d==1 ~ "1) Was not recommended",
          co3d==2 ~ "2) Was recommended",
          TRUE ~ NA_character_
        ), levels = c("1) Was not recommended", "2) Was recommended")),
        e = factor(case_when(
          co3e==1 ~ "1) Has concerns about adverse reactions",
          co3e==2 ~ "2) No concerns about adverse reactions",
          TRUE ~ NA_character_
        ), levels = c("1) Has concerns about adverse reactions", "2) No concerns about adverse reactions")),
        f = factor(case_when(
          co3f==1 ~ "1) Thinks vaccine is unsafe",
          co3f==2 ~ "2) Thinks vaccine is safe",
          TRUE ~ NA_character_
        ), levels = c("1) Thinks vaccine is unsafe", "2) Thinks vaccine is safe")),
        g = factor(case_when(
          co3g==1 ~ "1) Thinks vaccine won't protect",
          co3g==2 ~ "2) Thinks vaccine will protect",
          TRUE ~ NA_character_
        ), levels = c("1) Thinks vaccine won't protect", "2) Thinks vaccine will protect")),
        h = factor(case_when(
          co3h==1 ~ "1) Believes in long-term side effects",
          co3h==2 ~ "2) Doesn't believe in long-term side effects",
          TRUE ~ NA_character_
        ), levels = c("1) Believes in long-term side effects", "2) Doesn't believe in long-term side effects")),
        i = factor(case_when(
          co3i==1 ~ "1) No trust in government/medical authorities",
          co3i==2 ~ "2) Trusts in government/medical authorities",
          TRUE ~ NA_character_
        ), levels = c("1) No trust in government/medical authorities", "2) Trusts in government/medical authorities")),
        j = factor(case_when(
          co3j==1 ~ "1) No concerns about getting infected",
          co3j==2 ~ "2) Concerns about getting infected",
          TRUE ~ NA_character_
        ), levels = c("1) No concerns about getting infected", "2) Concerns about getting infected")),
        k = factor(case_when(
          co3k==1 ~ "1) Believes in natural immunity",
          co3k==2 ~ "2) Doesn't believe in natural immunity",
          TRUE ~ NA_character_
        ), levels = c("1) Believes in natural immunity", "2) Doesn't believe in natural immunity")),
        l = factor(case_when(
          co3l==1 ~ "1) Other reason",
          co3l==2 ~ "2) No other reason",
          TRUE ~ NA_character_
        ), levels = c("1) Other reason", "2) No other reason"))
      )
  }
  
}


