################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# Set a variable for the dynamic author in RMD files
set_author <- "WHO/EURO"
# Set a variable for the dynamic title in RMD files
set_title <- "STEPS KGZ Data Book"

################################################################################

# INCLUSION OF URBAN/RURAL AND REGION
# if TRUE - included
# if FALSE - excluded

# Write into the global environment 
.u_r_reg <<- TRUE
# OR
# .GlobalEnv$.u_r_reg <- TRUE

# Check the object
.u_r_reg

################################################################################

# R PACKAGES
list_of_packages <- c("here", "tidyverse", "RODBC", "odbc", "Hmisc", "rlang", "srvyr", "survey", 
                      "ggforce", "devEMF", "ggsci", "huxtable", "fs", "flextable", "writexl",
                      "rmarkdown", "markdown", "knitr", "magrittr", "plyr", "readxl", "svglite")

# Checking if already installed packages contain the required packages
if (length(setdiff(list_of_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(list_of_packages, rownames(installed.packages())), type = "binary")
}


################################################################################
# LOADING AND CLEANING DATA
################################################################################

# Identify project's working directory
library(here)

# Load other main packages
library(tidyverse)

################################################################################
# LOAD DATA 
################################################################################

datawt <- read.csv(here("Data","steps_kgz_datawt_anon_cln.csv"), encoding="UTF-8")

################################################################################
# CLEAN LOADED DATA
################################################################################

# Clean column names to lower case
datawt <- datawt %>% rename_all(tolower)

################################################################################
# CHECK LOADED DATA
################################################################################

# Core variable names that are used in the analysis
datawt_names <- c("sex", "c1", "valid", "wstep1", "wstep2", "wstep3", "psu", 
                  "stratum", "agerange", "agerange2", "ur", "region")

# Check which names are not in the data before proceeding
missing_names <- setdiff(datawt_names, names(datawt))

if (length(missing_names) > 0) {
  warning(sprintf("%s doesn't contain: %s. Creating the missing variables.", 
                  deparse(substitute(datawt)),
                  paste(missing_names, collapse=", ")))
  
  datawt <- datawt %>% 
    mutate(
      # Create correct agerange variable (should be two ranges in KGZ)
      agerange = factor(ifelse(age>=18 & age<=39, "18–39",
                               ifelse(age>=40 & age<=69, "40–69", NA))),
      # Create sex variable
      sex = factor(ifelse(c1.x == 1, "Men", "Women")),
      # Create stratum variable to include regional disaggregation
      # NOTE: For KGZ 2023, urban/rural disaggregation was used as strata for sampling
      stratum = factor(ifelse(!is.na(urban1rural2), urban1rural2, NA)),
      # Create psu variable to include clusters
      psu = factor(ifelse(!is.na(i1b.x), i1b.x, NA)),
      # Add agerange2 (18-39, 40-69) for Urban/Rural disaggregation
      agerange2 = factor(ifelse(age>=18 & age<=39, "18–39", 
                                ifelse(age>=40 & age<=69, "40–69", NA))),
      # Add region, ur variables as factors (specific for KGZ)
      # Region column based on provided information from the country
      region = factor(case_when(
        i1a.x == 'Oblast2' ~ 'Yssyk-Kul oblast',
        i1a.x == 'Oblast3' ~ 'Jalal-Abat oblast',
        i1a.x == 'Oblast4' ~ 'Naryn oblast',
        i1a.x == 'Oblast5' ~ 'Batken oblast',
        i1a.x == 'Oblast6' ~ 'Osh oblast',
        i1a.x == 'Oblast7' ~ 'Talas oblast',
        i1a.x == 'Oblast8' ~ 'Chui oblast',
        i1a.x == 'Oblast11' ~ 'Bishkek city',
        i1a.x == 'Oblast21' ~ 'Osh city',
        TRUE ~ NA_character_
      )),
      ur = factor(urban1rural2),
      # Recode values in ur with dplyr (specific for KGZ)
      ur = recode(ur, "1" = "Urban", "2" = "Rural"),
      # Create c1 variable after joining datasets for CardiovascularDiseaseRisk 
      # script as it requires c1 variable
      c1 = c1.x,
      # Create valid variable = consent signed, ages 18 to 69, and sex present
      valid = ifelse(i5 == 1 & (age >= 18 & age <= 69) 
                     & (sex == "Men" | sex == "Women"), 1, 2)
    )
    
  } else {
    
    # NOTE: agerange should be changed to a factor because sometimes age ranges are
    # dropped if there are 0 values in data and .drop = FALSE argument in group_by function
    # does not work if it is not a factor.
    # The same applies to variables agerange2, region, sex and ur
    
    datawt <- datawt %>% 
      mutate(
        agerange = factor(agerange),
        agerange2 = factor(agerange2),
        region = factor(region),
        ur = factor(ur),
        sex = factor(sex)
    )

  }

################################################################################

# Subset to valid
datawt <- subset(datawt, valid == 1)

#-------------------------------------------------------------------------------

# Check vars
# unique(datawt$agerange)
# unique(datawt$agerange2)
# unique(datawt$sex)
# unique(datawt$ur)
# unique(datawt$region)

# dim(datawt)
# table(datawt$valid)

################################################################################


