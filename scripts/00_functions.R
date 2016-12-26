
titles_clarifier <- function(vec) {
    # Clarifying sections titles. 
    #vec <- map_chr(vec, xmlValue)
    toSpace <- function(x, pattern) gsub(pattern, " ", x) # Fucntion to change characters to spaces.
    trim <- function (x) gsub("^\\s+|\\s+$", "", x) # Function to eliminate leading and trailing spaces. 
    toAnd <- function(x) gsub("(&|/|\\|)", "and", x) 
    
    vec <- sapply(vec, function(x) iconv(x, "latin1", "ASCII", sub="")) %>% 
        map_chr(tolower) %>%
        map_chr(toSpace, "\n") %>%
        map_chr(toSpace, "\r") %>% 
        map_chr(removeNumbers) %>% 
        map_chr(stripWhitespace) %>%
        map_chr(trim) %>% 
        map_chr(toAnd)
    return(vec)
}
