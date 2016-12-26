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
  
  if (!is.null(content$doc$children$article[["body"]])) {
    body_sections <- getNodeSet(content$doc$children$article[["body"]], "//sec")
    
    for (j in seq_along(body_sections)){
      section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")
      
      clarified_titles_vec <- titles_clarifier(xmlValue(section_title[[1]], recursive = FALSE))
      
      
      if (length(clarified_titles_vec) > 0) {
        if (clarified_titles_vec %in% possible_titles) {
          section <- getNodeSet(body_sections[[j]], "/sec")
          section_vec <- append(section_vec, xmlValue(section[[1]]))
          break()
        }
      }
    }
  }
  closeAllConnections() # Fixes the xmlValue bug related to too many textConnections opened. 
  print(i/length(files) * 100)
}


regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"

one <- str_extract(section_vec,regexp)


