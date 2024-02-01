################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

mheartrate <- function(.data) {
  
  # variable names that are used in the function
  mheartrate_names <- c("sex", "m16a", "m16b", "m16c", "m16c")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mheartrate_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mheartrate_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(
        m16acln = if_else(m16a>=30 & m16a<=200, 1, 2, missing = 2),
        m16bcln = if_else(m16b>=30 & m16b<=200, 1, 2, missing = 2),
        m16ccln = if_else(m16c>=30 & m16c<=200, 1, 2, missing = 2),
        rate = case_when(
          m16acln==1 & m16bcln==1 & m16ccln==1 ~ (m16a+m16b+m16c)/3,
          m16acln==2 & m16bcln==1 & m16ccln==1 ~ (m16b+m16c)/2,
          m16acln==1 & m16bcln==2 & m16ccln==1 ~ (m16a+m16c)/2,
          m16acln==1 & m16bcln==1 & m16ccln==2 ~ (m16a+m16b)/2,
          m16acln==1 & m16bcln==2 & m16ccln==2 ~ m16a,
          m16acln==2 & m16bcln==1 & m16ccln==2 ~ m16b,
          m16acln==2 & m16bcln==2 & m16ccln==1 ~ m16c,
          TRUE ~ NA_real_
        ),
        cln = if_else(rate>=30 & rate<=200 & valid==1, 1, 2, missing = 2),
        c = factor(if_else(rate<=100, "beats per minute <=100", 
                           "beats per minute over 100", missing = "beats per minute over 100"),
                   levels = c("beats per minute <=100", "beats per minute over 100"))
      )
  }
}
