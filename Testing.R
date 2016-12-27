files <- list.files("./corpus", full.names = TRUE)

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
          section_vec <- append(section_vec, xmlValue(section[[1]]))
          break()
        }
      }
    } 
  }
  
  #print(i/length(files) * 100)
}

regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_df <- as_tibble(str_extract(section_vec,regexp))
length(which(!is.na(p_values_df)))

p_values_df <- p_values_df %>%
  separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]") 

write.csv(p_values_df, "./data/p-values_df.cvs")


