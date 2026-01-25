# load_update_workflow.R - Example workflow using modular pipeline
# Demonstrates Phase 3 modular design for setlist prediction

library(tidyverse)

# Load modular pipeline
source('./Code/data_pipeline.R')

###############################################################################
# WORKFLOW 0: Load + Save Data
###############################################################################
wsp_list <- load_all_data(use_cache = TRUE)
save_setlists()