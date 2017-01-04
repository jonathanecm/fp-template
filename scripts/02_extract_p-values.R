##############################################
# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.
##############################################
# Extracts p-values from abstract or results 
# section, abbreviated journal name, number of 
# authors, article title, symbols or operator, 
# and publication year. 
# "./data/sampled_corpus" folder 
# *** THIS SCRIPT IS NOT IMPLEMENTE FOR THE ***
#     ANALYSIS BECAUSE THE FULL CORPUS WAS NOT
#     UPLOADED TO THE REMORE REPOSITORY BECAUSE
#     IT SIZE ON DISK: 107.52 GB.
##############################################

# Libraries
library(tm)
library(tidyverse)
library(tidytext)
library(XML)
library(stringr)
source("./scripts/00_functions.R") # Call to functions script. 

# Variables 
files <- list.files("./data/sampled_corpus", full.names = TRUE)
autors_number_vec <- vector()
section_vec <- vector()
article_name_vec <- vector()
journal_vec <- vector()
abstract_vec <- vector()
pub_year_vec <- vector()
title_vec <- vector()
subject_vec <- vector() 
abbrev_journal_name_vec <- vector()

# Variable with possible results section titles. 
possible_titles <- c('results', 'results and discussion', 'methods and results', 'results and discussions',
                     'results, discussion and conclusions', 'discussion and results',
                     'study results', 'results and conclusions', 'results and conclusion',
                     'experimental results', 'observations and results',
                     'results and observations', 'results and observation',
                     'observations, results and discussion', 'empirical results', 'materials and results',
                     'experimental procedures and results', 'research results', 'analysis and results',
                     'results and analysis', 'methodsandresults', 'resultsanddiscussion', 'resultsandconclusions',
                     'method and results')


for (i in seq_along(files)){                      # Iterates over corpus. 
  
  content <- xmlTreeParse(files[i]) 
  
  if (!is.null(content$doc$children$article)) {   # Checks that xXML article section is not NULL.   
    
    body_sections <- getNodeSet(content$doc$children$article, "//sec")  # Extracts nXML section to process.
    closeAllConnections()                         # Fixes the xmlValue bug related to too many textConnections opened. 
    
    for (j in seq_along(body_sections)){          # Iterates over extracted nXML section. 
      
      section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")
      
      # Calls to text_clarifier function for clarifying section title. 
      clarified_titles_vec <- text_clarifier(xmlValue(section_title[[1]], recursive = FALSE))
      
      if (length(clarified_titles_vec) > 0) {     # Checks that clarified section is not NULL.
    
        if (clarified_titles_vec %in% possible_titles) {  # Determines if is result section.   

          section <- getNodeSet(body_sections[[j]], "/sec")
          section_vec <- append(section_vec, xmlValue(section[[1]])) # Extracts result section. 
          
          # Extract abbreviated journal name. 
          abbrev_journal_name <- getNodeSet(content$doc$children$article, "//journal-id[@journal-id-type='iso-abbrev']")
          if (!is_empty(abbrev_journal_name)) {
            abbrev_journal_name_vec <- append(abbrev_journal_name_vec, xmlValue(abbrev_journal_name[[1]]))
          } else {
            abbrev_journal_name_vec <- append(abbrev_journal_name_vec, "NA")
          }
          
          # Extract abstract.
          abstract <- getNodeSet(content$doc$children$article, "//abstract")
          if (!is_empty(abstract)) {
            abstract_vec <- append(abstract_vec, xmlValue(abstract[[1]]))
          } else {
            abstract_vec <- append(abstract_vec, "NA")
          }
          
          # Extract title. 
          title <- basename(content$doc$file)
          if (!is_empty(title)) {
            title_vec <- append(title_vec, title)
          } else {
            title_vec <- append(title_vec, "NA")
          }
          
          # Extract Number of authors.
          autors <- length(getNodeSet(content$doc$children$article, "//contrib")) 
          autors_number_vec <- append(autors_number_vec, autors)
          
          # Gets year of publication.
          pub_year <- getNodeSet(content$doc$children$article[["front"]][["article-meta"]][["pub-date"]], "//year")
          pub_year_vec <- append(pub_year_vec, xmlValue(pub_year[[1]]))
          
          break()
        }
      }
    } 
  }
  print("Percent of files processed:")
  print(i/length(files) * 100)
}

# Extract p-values and symbols into a data frame. 
p_remover <- function (x) gsub("[Pp]", "", x) # removes the p character from the string. 

# Extract p-values from result section and removes unwanted character and white spaces from p-value. 
regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_vec <- as.vector(str_extract(section_vec, regexp)) %>%
  map_chr(p_remover) %>% 
  str_replace_all(" ", "") %>%
  stripWhitespace() %>%
  str_replace_all(" ", "") 

# # Extractig p-values form abstracts and removes unwanted character and white spaces from p-value.
regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
abstract_p_val_vec  <- as.vector(str_extract(abstract_vec, regexp)) %>%
  map_chr(p_remover) %>%
  str_replace_all(" ", "") %>%
  stripWhitespace() %>%
  str_replace_all(" ", "")

# Create data frame for extracted information.
p_values_df <- tibble(p_values_vec, abstract_p_val_vec, autors_number_vec, 
                      pub_year_vec, title_vec, abbrev_journal_name_vec)

names(p_values_df) <- c("result", "abstract", "num_authors", 
                        "pub_year", "title", "abbrev_journal_name")  

# Removes operator from p-values.
symbols_remover <- function (x) gsub("[=<>≤≥]", "", x)
p_values_df <-p_values_df %>% 
  gather(`result`, `abstract`, key = "section", value = "p-values") %>%
  mutate(symbols = str_extract(`p-values`, "[=<>≤≥]"), 
        `p-values` = as.double(map_chr(`p-values`, symbols_remover)))

# save data frame.
write.csv(p_values_df, "./data/raw/p-values_df.csv")



