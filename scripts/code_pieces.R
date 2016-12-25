element <- c("address", "direction", "street")

first <- function(x) {element[1]}
sapply(element, element[[1]])

string <- "23 mai 2000"
string2 <- "1 mai 2000"
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
grepl(pattern = regexp, x = string)

library("stringr")
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
string <- "blabla 23 mai 2000 blabla 18 mai 2004"
str_extract(string,regexp)
"23 mai 2000"
str_extract_all(string,regexp)


string <- "The growing trend of using smartphones as personal computing platforms to access and store private information has stressed the demand for secure and usable authentication mechanisms. This paper investigates the feasibility and applicability of using motion-sensor behavior data for user authentication on smartphones. For each sample of the passcode, sensory data from motion sensors are analyzed to extract descriptive and intensive features for accurate and fine-grained characterization of users’ passcode-input actions. One-class learning methods are applied to the feature space for performing user authentication. Analyses are conducted using data from 48 participants with 129,621 passcode samples across various operational scenarios and different types of smartphones. Extensive experiments are included to examine the efficacy of the proposed approach, which achieves a false-rejection rate of 6.85% and a false-acceptance rate of 5.01%. Additional experiments on usability with resp... <truncated> p < .05, some p values are reported as p < .001'"



extractor <- function(vec) {
    regexp <- "[^a-zA-Z0-9*#†_][Pp][\\s]*[=<>≤≥][\\s]*[01]?[.][\\d]+"
    str_extract_all(string,regexp)
    grepl(pattern = regexp, x = string)
    }

map(abstract_vec, )






