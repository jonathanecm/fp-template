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

# Extracting p-values (exmplain regex expression)
regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_df <- as_tibble(str_extract(section_vec,regexp)) # Extracts the first p-value per section. 

# Extrall all p-values per section.
#test <- as.vector(str_extract_all(section_vec, regexp)) # Extrall all p-values per section.

# cleanning p-values
p_remover <- function (x) gsub("[Pp]", "", x)
p_values_df[1] <- p_values_df %>% map_chr(p_values_df$value, p_remover) %>% 
  
  separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]") 











test <- str_extract(section_vec, regexp) 


cleaned <- str_replace_all(test, " ", "")
cleaned <- str_replace_all(cleaned, " ", "")



trim2 <- function (x) gsub(" ", "", x)
trim3 <- function (x) gsub(" ", "", x)
(cleaned <- map_chr(test, trim3))
(cleaned <- map_chr(cleaned, trim2))

(cleaned <- map_chr(cleaned, stripWhitespace)) 

#length(which(!is.na(p_values_df)))






# t <- vector()
# for (i in seq_along(tp_values_df)) {
#   t <- as.character(tp_values_df[3][1])
# }


trim2 <- function (x) gsub("[Pp]", "", x)
(cleaned <- map_chr(p_values_df$value, trim2))

(cleaned2 <- as_tibble(str_extract(cleaned, "[=<>≤≥]")))


separate(claened, c("Symbol", "p_value"), sep = "[=<>≤≥]")  



result <- p_values_df %>% 
  as_tibble(str_extract(section_vec,regexp))

result <- as_tibble(map_chr(result$value, trim2)) 
final <- separate(result$ value, c("Symbol", "p_value"), sep = "[=<>≤≥]") #%>%
  


p_values_df <- p_values_df %>%

  
separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]")  



write.csv(p_values_df, "./data/p-values_df.cvs")



p_remover <- function (x) gsub("[Pp]", "", x) # removes p from the string 

regexp <- "[Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
p_values_vec <- as.vector(str_extract(section_vec, regexp)) %>%
  map_chr(p_remover) %>%
  str_replace_all(" ", "") %>%
  stripWhitespace() %>%
  str_replace_all(" ", "") 

symbols <- str_extract(p_values_vec, "[=<>≤≥]") 

p_values_df <- p_values_vec %>% 
  as_tibble() %>%
  separate(value, c("Symbol", "p_value"), sep = "[=<>≤≥]") 

p_values_df$Symbol <- str_extract(p_values_vec, "[=<>≤≥]") 


# t <- vector()
# for (i in seq_along(tp_values_df)) {
#   t <- as.character(tp_values_df[3][1])
# }




write.csv(p_values_df, "./data/p-values_df.cvs")






