files <- list.files("./corpus", full.names = TRUE)
autors_number_vec <- vector()
section_vec <- vector()
article_name_vec <- vector()
journal_vec <- vector()
abstract_vec <- vector()
pub_year_vec <- vector()

for (i in seq_along(files)){
  
  content <- xmlTreeParse(files[i]) 
  print(i)
  if (!is.null(content$doc$children$article)) {
    body_sections <- getNodeSet(content$doc$children$article, "//sec")
    
    closeAllConnections() # Fixes the xmlValue bug related to too many textConnections opened. 
    for (j in seq_along(body_sections)){
      section_title <- getNodeSet(body_sections[[j]][["title"]], "//title")
      
      clarified_titles_vec <- text_clarifier(xmlValue(section_title[[1]], recursive = FALSE))
      
      if (length(clarified_titles_vec) > 0) {
        #print(clarified_titles_vec)
        if (clarified_titles_vec %in% possible_titles) {
          #browser()
          section <- getNodeSet(body_sections[[j]], "/sec")
          section_vec <- append(section_vec, xmlValue(section[[1]])) # Extracts result section. 
          article_name_vec <- append(article_name_vec, content$doc$file[[1]]) # Extracts article full name including folder. 
          
          journal <- getNodeSet(content$doc$children$article, "//journal-title")
          journal_vec <- append(journal_vec, xmlValue(journal[[1]])) # Extracts result section. 
        
          # Extract abtract.
          abstract <- getNodeSet(content$doc$children$article, "//abstract")
          if (!is_empty(abstract)) {
            abstract_vec <- append(abstract_vec, xmlValue(abstract[[1]]))
          } else {
            abstract_vec <- append(abstract_vec, "NA")
          }
          
          # Number of authors.
          autors <- length(getNodeSet(content$doc$children$article, "//contrib")) 
          autors_number_vec <- append(autors_number_vec, autors)
          
          # Get year of publication
          pub_year <- getNodeSet(content$doc$children$article, "//pub-date")
          pub_year_vec <- append(pub_year_vec, xmlValue(pub_year[[1]]))
          
      break()
        }
      }
    } 
  }
  
  #print(i/length(files) * 100)
}

regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
test <- as_vector(str_extract_all(section_vec, regexp))
tp_values_df <- as_tibble(str_extract_all(section_vec,regexp))
#length(which(!is.na(p_values_df)))

p_values_df <- p_values_df %>%
  separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]") 

write.csv(p_values_df, "./data/p-values_df.cvs")

t <- vector()
(t <- append(t, list_ps[[3]]))
length(test[[3]])







