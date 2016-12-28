# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.

# Extractor:
# This script #### 

# Libraries ####
library(tm)
library(tidyverse)
library(tidytext)
library(XML)
library(stringr)
source("./scripts/00_functions.R")

# Variables 
files <- list.files("./corpus", full.names = TRUE)
section_vec <- vector()
article_name_vec <- vector()
journal_vec <- vector()
abstract_vec <- vector()
possible_titles <- c('results', 'results and discussion', 'methods and results', 'results and discussions',
                     'results, discussion and conclusions', 'discussion and results',
                     'study results', 'results and conclusions', 'results and conclusion',
                     'experimental results', 'observations and results',
                     'results and observations', 'results and observation',
                     'observations, results and discussion', 'empirical results', 'materials and results',
                     'experimental procedures and results', 'research results', 'analysis and results',
                     'results and analysis', 'methodsandresults', 'resultsanddiscussion', 'resultsandconclusions',
                     'method and results')

# Files location.
#files_path <- "./corpus" 


# Creating a corpus. Using corpus to avoid textConnetion problem.
#corpus <- Corpus(DirSource(files_path), readerControl = list(reader = readReut21578XML)) 

# Defining possible titles of the result section. 


## Extracting results section.
#Covert this to a map function.
for (i in seq_along(files)){
  
  content <- xmlTreeParse(files[i]) 
  print(i)
  if (!is.null(content$doc$children$article[["body"]])) {
    body_sections <- getNodeSet(content$doc$children$article[["body"]], "//sec")
    
    closeAllConnections() # Fixes the xmlValue bug related to too many textConnections opened. 
    for (j in seq_along(body_sections)){
      section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")
      
      clarified_titles_vec <- text_clarifier(xmlValue(section_title[[1]], recursive = FALSE))
      
      if (length(clarified_titles_vec) > 0) {
        if (clarified_titles_vec %in% possible_titles) {
          braws
          section <- getNodeSet(body_sections[[j]], "/sec")
          section_vec <- append(section_vec, xmlValue(section[[1]]))
          break()
        }
      }
    }
  }
  
  #print(i/length(files) * 100)
}

# Extractig p-values from results section. 
regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_df <- as_tibble(str_extract(section_vec,regexp))
length(which(!is.na(p_values_vec)))

p_values_df <- p_values_df %>%
  separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]") 

write.csv(p_values_df, "./data/p-values_df.cvs")


