################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

hlifestyle <- function(.data) {
  
  hlifestyle_names <- c("sex", "h20a", "h20b", "h20c", "h20d", "h20e", "h20f", "h20g", "h20")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hlifestyle_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hlifestyle_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(
        h20cln = if_else((h20==1 | h20==2) & valid==1, 1, 2, missing = 2),
        v = factor(ifelse(h20==1, "visited a doctor or other health worker", 
                          ifelse(h20==2, "not visited a doctor or other health worker", NA)),
                   levels = c("visited a doctor or other health worker",
                              "not visited a doctor or other health worker")),
        h20acln = if_else((h20a==1 | h20a==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20bcln = if_else((h20b==1 | h20b==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20ccln = if_else((h20c==1 | h20c==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20dcln = if_else((h20d==1 | h20d==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20ecln = if_else((h20e==1 | h20e==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20fcln = if_else((h20f==1 | h20f==2) & h20==1 & valid==1, 1, 2, missing = 2),
        h20gcln = if_else((h20g==1 | h20g==2) & h20==1 & valid==1, 1, 2, missing = 2),
        a = factor(ifelse(h20a==1, "advised to quit/not start tob", 
                          ifelse(h20a==2, "not advised to quit/not start tob", NA)),
                   levels = c("advised to quit/not start tob", 
                              "not advised to quit/not start tob")),
        b = factor(ifelse(h20b==1, "advised to reduce salt", 
                          ifelse(h20b==2, "not advised to reduce salt", NA)),
                   levels = c("advised to reduce salt", 
                              "not advised to reduce salt")),
        c = factor(ifelse(h20c==1, "advised to eat more fruit/veg", 
                          ifelse(h20c==2, "not advised to eat more fruit/veg", NA)),
                   levels = c("advised to eat more fruit/veg", 
                              "not advised to eat more fruit/veg")),
        d = factor(ifelse(h20d==1, "advised to reduce fat in diet", 
                          ifelse(h20d==2, "not advised to reduce fat in diet", NA)),
                   levels = c("advised to reduce fat in diet", 
                              "not advised to reduce fat in diet")),
        e = factor(ifelse(h20e==1, "advised to do more physical activity", 
                          ifelse(h20e==2, "not advised to do more physical activity", NA)),
                   levels = c("advised to do more physical activity", 
                              "not advised to do more physical activity")),
        f = factor(ifelse(h20f==1, "advised to maintain/lose weight", 
                          ifelse(h20f==2, "not advised to maintain/lose weight", NA)),
                   levels = c("advised to maintain/lose weight", 
                              "not advised to maintain/lose weight")),
        g = factor(ifelse(h20g==1, "advised to reduce sugary beverages in diet",
                          ifelse(h20g==2, "not advised to reduce sugary beverages in diet", NA)),
                   levels = c("advised to reduce sugary beverages in diet", 
                              "not advised to reduce sugary beverages in diet"))
      )
  }
  
}
