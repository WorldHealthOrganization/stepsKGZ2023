################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

tsmoketypeprev <- function(.data) {
  
  # variable names that are used in the function
  tsmoketypeprev_names <- c("sex", "t1", "t2", "t5a", "t5aw", "t5b", "t5bw", 
                            "t5c", "t5cw", "t5d", "t5dw", "t5e", "t5ew", 
                            "t5f", "t5fw", "t5g", "t5gw")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmoketypeprev_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmoketypeprev_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2),
        t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 2, missing = 2),
        t2cln = replace(t2cln, t1==2 & (is.na(t2) | t2==2), 1),
        cln = if_else(t1cln==1 & t2cln==1 & valid==1, 1, 2, missing = 2),
        c = factor(if_else((t2==1 & t5a>0 & t5a<=50) | (t5aw>0 & t5aw<=350), "1) smokes manuf cigs", 
                           "2) does not smoke manuf cigs", missing = "2) does not smoke manuf cigs"),
                   levels = c("1) smokes manuf cigs", "2) does not smoke manuf cigs")),
        cd = factor(if_else((t2==1 & t5a>0 & t5a<=50), "1) smokes manuf cigs daily", 
                            "2) does not smoke manuf cigs daily", missing = "2) does not smoke manuf cigs daily"),
                    levels = c("1) smokes manuf cigs daily", "2) does not smoke manuf cigs daily")),
        d = factor(if_else((t2==1 & t5b>0 & t5b<=50) | (t5bw>0 & t5bw<=350), "1) smokes hand-rolled cigs", 
                           "2) does not smoke hand-rolled cigs", missing = "2) does not smoke hand-rolled cigs"),
                   levels = c("1) smokes hand-rolled cigs", "2) does not smoke hand-rolled cigs")),
        dd = factor(if_else((t2==1 & t5b>0 & t5b<=50), "1) smokes hand-rolled cigs daily", 
                            "2) does not smoke hand-rolled cigs daily",
                            missing = "2) does not smoke hand-rolled cigs daily"),
                    levels = c("1) smokes hand-rolled cigs daily", 
                               "2) does not smoke hand-rolled cigs daily")),
        e = factor(if_else((t2==1 & t5c>0 & t5c<=50) | (t5cw>0 & t5cw<=350), "1) smokes pipes", 
                           "2) does not smoke pipes", missing = "2) does not smoke pipes"),
                   levels = c("1) smokes pipes", "2) does not smoke pipes")),
        ed = factor(if_else((t2==1 & t5c>0 & t5c<=50), "1) smokes pipes daily", 
                            "2) does not smoke pipes daily", missing = "2) does not smoke pipes daily"),
                    levels = c("1) smokes pipes daily", "2) does not smoke pipes daily")),
        f = factor(if_else((t2==1 & t5d>0 & t5d<=50) | (t5dw>0 & t5dw<=350), "1) smokes cigars", 
                           "2) does not smoke cigars", missing = "2) does not smoke cigars"),
                   levels = c("1) smokes cigars", "2) does not smoke cigars")),
        fd = factor(if_else((t2==1 & t5d>0 & t5d<=50), "1) smokes cigars daily",
                            "2) does not smoke cigars daily", missing = "2) does not smoke cigars daily"),
                    levels = c("1) smokes cigars daily", "2) does not smoke cigars daily")),
        g = factor(if_else((t2==1 & t5e>0 & t5e<=50) | (t5ew>0 & t5ew<=350), "1) smokes shisha", 
                           "2) does not smoke shisha", missing = "2) does not smoke shisha"),
                   levels = c("1) smokes shisha", "2) does not smoke shisha")),
        gd = factor(if_else((t2==1 & t5e>0 & t5e<=50), "1) smokes shisha daily", 
                            "2) does not smoke shisha daily", missing = "2) does not smoke shisha daily"),
                    levels = c("1) smokes shisha daily", "2) does not smoke shisha daily")),
        h = factor(if_else((t2==1 & t5f>0 & t5f<=50) | (t5fw>0 & t5fw<=350), 
                           "1) smokes heated tobacco products (iQOS, etc.)", 
                           "2) does not smoke heated tobacco products (iQOS, etc.)", 
                           missing = "2) does not smoke heated tobacco products (iQOS, etc.)"),
                   levels = c("1) smokes heated tobacco products (iQOS, etc.)", 
                              "2) does not smoke heated tobacco products (iQOS, etc.)")),
        hd = factor(if_else((t2==1 & t5f>0 & t5f<=50), 
                            "1) smokes heated tobacco products (iQOS, etc.) daily", 
                            "2) does not smoke heated tobacco products (iQOS, etc.) daily", 
                            missing = "2) does not smoke heated tobacco products (iQOS, etc.) daily"),
                    levels = c("1) smokes heated tobacco products (iQOS, etc.) daily", 
                               "2) does not smoke heated tobacco products (iQOS, etc.) daily")),
        i = factor(if_else((t2==1 & t5g>0 & t5g<=50) | (t5gw>0 & t5gw<=350), 
                           "1) smokes other type of tobacco", 
                           "2) does not smoke other type of tobacco", 
                           missing = "2) does not smoke other type of tobacco"),
                   levels = c("1) smokes other type of tobacco", 
                              "2) does not smoke other type of tobacco")),
        id = factor(if_else((t2==1 & t5g>0 & t5g<=50), "1) smokes other type of tobacco daily", 
                            "2) does not smoke other type of tobacco daily", 
                            missing = "2) does not smoke other type of tobacco daily"),
                    levels = c("1) smokes other type of tobacco daily", 
                               "2) does not smoke other type of tobacco daily"))
      )
    
  }
  
}
