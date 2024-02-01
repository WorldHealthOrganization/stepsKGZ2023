################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Affiliations: WHO/EURO Special Initiative on Noncommunicable Diseases and Innovation
# Copyright: CC BY-NC-SA 3.0 IGO
################################################################################

# load CSV data files for joining into one df for the fact sheet

# load packages
library(here)
library(tidyverse)
library(fs) # for using dir_ls function

fs_df <- here("FactSheet") %>% 
  # read in only CSV files
  dir_ls(regexp = "\\.csv$") %>% 
  # scale up to all CSV files with a map functions from purrr
  map_dfr(read_csv)

fs_df


# create a Word table
library(huxtable)
fs_df_hux <- hux(fs_df)
width(fs_df_hux) <- 1
fs_df_hux <- set_header_rows(fs_df_hux, 1, TRUE)
fs_df_hux <- style_headers(fs_df_hux, bold = TRUE)
caption(fs_df_hux) <- "STEPS KGZ 2023 Fact sheet"
quick_docx(set_all_borders(fs_df_hux), file = ("FactSheet/fs_kgz_version_en.docx"))



