###########################################################
# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.
###########################################################
# Randomly sampled nXML files from "./data/full_corpus".
# and saves the sampled nXML files into the directory
# "./data/sampled_corpus".
# *** FOR A 107.52 GB. CORPUS THE RUN TIME IS 17 HOURS ***
###########################################################

# Libraries
library(tidyverse)

# Create a list of files' names.
full_corpus_path <- file.path(".", "data", "full_corpus") %>% 
  list.files(full.names = TRUE, recursive = TRUE)

# Takes a sample of files' names.
sampled_files <- sample(full_corpus_path, 100000) # This number can be ajusted to the desire
files_names <- basename(sampled_files)            # sample size. 

# Creates a corpus of sampled files in the "./data/sampled_corpus" directory. 
destination_files <- file.path(".", "data", "sampled_corpus", files_names)
file.copy(sampled_files, destination_files, overwrite = FALSE)

# Removing variables to save memory. 
rm(full_corpus_path)
rm(destination_files)
