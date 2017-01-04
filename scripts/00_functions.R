###############################################################
# COMPUTING FOR THE SOCIAL SCIENCES
# FINAL PROJECT 
# Camacho Jonathan E.
###############################################################
# This script contains a series of helper functions:
# text_clarifier: Clarifies text.
# bootstrap.binomial.bias.test: Carries out a binomial test
# binomial.bias.test: Creates the limits for the binomial test
###############################################################

text_clarifier <- function(vec) {
##########################################################################
# Perform text transformations to clarifies the text from blank spaces, 
#  non-ASCII characters, and extraneous characters such as "\n" and "\r".
##########################################################################
# imput:      vec, character vector, a vector with strings to be clarified. 
# output:     vec, clarified string vector. 
##########################################################################
    
  toSpace <- function(x, pattern) gsub(pattern, " ", x)   # Fucntion to change characters to spaces.
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)         # Function to eliminate leading and trailing spaces. 
    toAnd <- function(x) gsub("(&|/|\\|)", "and", x)        # Removes the characters in regex expression. 
    
# Other R predefined text transformations. 
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


bootstrap.binomial.bias.test <- function(df, replicates){ # recives the full dataframe and the number of replications. 
  ######################################################################################
  # Performs a boostrapped binomial test analysis.  
  ######################################################################################
  # imput:      df, data frame, p-values data frame from results or abstract secctions. 
  # output:     boot.results, data frame, data frame with results from binomial test. 
  ######################################################################################
  
  lower.limits <- c(0.03, 0.04)             # limits of p-values to run the binomial test.
  len <- length(lower.limits) * replicates  # Variable to set the number of rows of the results dataframe. 
  
  # Creates a result data frame.
  results <- data.frame("lower.limit" = rep(lower.limits, each = replicates), 
                        "binomial.bias.lower"=numeric(len), "binomial.bias.higher"=numeric(len), 
                        "binomial.bias.p"=numeric(len))
  
  for(i in 1:2){              # Interate over the two lower limits 0.03, 0.04.
    for(j in 1:replicates) {  # Iterate over the number of replications
      results[(j + replicates*(i-1)), ] <- c(lower.limits[i], 
                                             binomial.bias.test(df$p.value, # calls binomial.bias.test function.
                                                                lower.limit= lower.limits[i], run.test=F))
    }
  }
  
  # Creates a data frame for test result. 
  boot.results <- data.frame(lower.limit = lower.limits, binomial.bias.lower = numeric(2), 
                             binomial.bias.higher = numeric(2), binomial.bias.p = numeric(2))
  
  # Adds results values in to boots.results data frame. 
  for(i in 2:3) boot.results[,i] <- as.numeric(tapply(results[,i], results$lower.limit, mean))
  
  # Performes binomial test of all p-values. 
  for(i in 1:2) boot.results[i,4] <- binom.test(c(round(boot.results$binomial.bias.higher[i]), 
                                                  round(boot.results$binomial.bias.lower[i])))$p.value 
  
  return(boot.results)
}


binomial.bias.test <- function(p, lower.limit=0.03, run.test=T) {
  ###########################################################################
  # Creates a data frame with parameters for the binomial test. 
  ###########################################################################
  # imput:      p, vec, a vector with p-values. 
  # output:     d, data frame, data frame wiht high and lower limits for test. 
  ###########################################################################
  
  # Discriminates between the two set limits 0.03 or 0.04 to select p-values.
  if(lower.limit==0.03){
    limits = c(0.03, 0.05)
    midpoint <- 0.04 # create mid-point.
    lower <- length(which(p<midpoint & p>=limits[1]))   # Determines lenght of lower limit. 
    higher <- length(which(p<limits[2] & p>=midpoint))  # Determine the lenght of higher limits. 
  } else if(lower.limit==0.04){
    limits = c(0.04, 0.05)
    midpoint <- 0.045 # Creates mid-point.
    lower <- length(which(p<midpoint & p>limits[1]))    # Determines lenght of lower limit.
    higher <- length(which(p<limits[2] & p>midpoint))   # Determine the lenght of higher limits. 
  } else{
    stop("lower.limit must be set to either 0.03 or 0.04")
  }
  
  d <- data.frame("lower"=lower, "higher"=higher, "p"=NA) 
  
  return(d)
}

# Bootstraping binomial test fot categoties. 
bootstrap.FoR.test <- function(df, replicates){
  ###########################################################################
  # Bootstrap replications for conduncting binomial test. 
  ###########################################################################
  # imput:      df, data frame, data frame with p-values. 
  # output:     boot.results, data frame, for thr results of binomial test by cat. 
  ###########################################################################
  results <- run.tests.FoRCode(df, run.test=F)
  boot.results <- results
  
  # Creates data frame for resutls. 
  place.holder <- data.frame(category = rep(results$category, replicates-1), 
                             binomial.bias.p=NA, binomial.bias.lower=NA, 
                             binomial.bias.higher=NA, binomial2.bias.p=NA, 
                             binomial2.bias.lower=NA, binomial2.bias.higher=NA, 
                             binomial.all.p=NA, binomial.all.lower=NA, 
                             binomial.all.higher=NA, binomial2.effectsize=NA, 
                             binomial.effectsize=NA, binomial.all.effectsize=NA)
  
  # Determines number of categories. 
  n.cat <- length(results$category)
  results <- rbind(results, place.holder)
  start <- 1 + n.cat * (2:replicates - 1)
  end <- 2:replicates * n.cat
  
  # Iterates over number of replications. 
  for(i in 2:replicates){ 
    results[start[i-1]:end[i-1], ] <- run.tests.FoRCode(df, run.test=F)
  }
  
  results$category <- factor(results$category, levels = results$category[1:n.cat])

  # Calculate average among bootstraped replications. 
  for(i in 2:13)	boot.results[,i] <- as.numeric(tapply(results[,i], results$category, mean))    
  
  # Calculates binomial tests according tobootstrapped numbers of p values.
  for(i in 1:nrow(boot.results)){     
    boot.results[i,"binomial.bias.p"] <- binom.test(c(round(boot.results$binomial.bias.higher[i]), 
                                                      round(boot.results$binomial.bias.lower[i])), 
                                                    alternative = "greater")$p.value 
    
    boot.results[i,"binomial2.bias.p"] <- binom.test(c(round(boot.results$binomial2.bias.higher[i]), 
                                                       round(boot.results$binomial2.bias.lower[i])), 
                                                     alternative = "greater")$p.value 
    
    boot.results[i,"binomial.all.p"] <- binom.test(c(round(boot.results$binomial.all.higher[i]), 
                                                     round(boot.results$binomial.all.lower[i])))$p.value 
  }
  
  return(boot.results)
}

# Two tailed binomial test on the whole dataset
binomial.all.test <- function(p, run.test=T) {
  ###########################################################################
  # Performs binomial test. 
  ###########################################################################
  # imput:      p, data frame, data frame with p-values. 
  # output:     d, data frame, with results of binomial test.
  ###########################################################################  
 
  # Sets limits for test. 
  midpoint <- 0.025
  
  higher <- sum(p>midpoint)
  lower <- sum(p<midpoint)
  
  # Performs test. 
  if(run.test == T) {r <- binom.test(c(higher,lower)) 
  d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
  }
  else d <- data.frame("lower"=lower, "higher"=higher, "p"=NA)
  
  return(d)
}


run.tests.FoRCode <- function(e, run.test = T){
  ###########################################################################
  # Performs binomial test for all categories. 
  ###########################################################################
  # imput:      e, data frame, data frame with p-values. 
  # output:     results, data frame, with results of binomial test for all cat.
  ########################################################################### 
  
  # Creates data frame. 
  results <- data.frame("category"=numeric(0), "binomial.bias.p"=numeric(0), 
                        "binomial.bias.lower"=numeric(0), 
                        "binomial.bias.higher"=numeric(0), 
                        "binomial2.bias.p"=numeric(0), 
                        "binomial2.bias.lower"=numeric(0), 
                        "binomial2.bias.higher"=numeric(0), 
                        "binomial.all.p"=numeric(0), 
                        "binomial.all.lower"=numeric(0), 
                        "binomial.all.higher"=numeric(0))
  
  # Iterates unique categories. 
  for(j in unique(as.character(e$FoRCode))){
    b.p <- NA
    a.p <- NA
    c.p <- NA
    
    p.vals <- e$p.value[which(e$FoRCode==j)]
    
    # Perform binomial test according to limits and for all p-values. 
    tryCatch(c.p <- binomial.bias.test(p.vals, lower.limit = 0.04, run.test = run.test), 
             error = function(e) c.p <- NA)
    
    tryCatch(b.p <- binomial.bias.test(p.vals, lower.limit = 0.03, run.test = run.test), 
             error = function(e) b.p <- NA)
    
    tryCatch(a.p <- binomial.all.test(p.vals, run.test = run.test), 
             error = function(e) a.p <- NA)
    
    # Conditional to check in the case that there are not p-values for the determined limits. 
    if(is.na(c.p)){
      c.p$p <- NA
      c.p$lower <- 0
      c.p$higher <- 0
    }
    if(is.na(b.p)){
      b.p$p <- NA
      b.p$lower <- 0
      b.p$higher <- 0
    }
    if(is.na(a.p)){
      a.p$p <- NA
      a.p$lower <- 0
      a.p$higher <- 0
    }
    
    # Fills the results data frame with results binomial test. 
    results[nrow(results)+1,] <- c(as.character(j), b.p$p, b.p$lower, b.p$higher, 
                                   c.p$p, c.p$lower, c.p$higher, a.p$p, a.p$lower, 
                                   a.p$higher)
  }
  results$binomial.all.p <- as.numeric(as.character(results$binomial.all.p))   
  results$binomial.all.lower <- as.numeric(as.character(results$binomial.all.lower))
  results$binomial.all.higher <- as.numeric(as.character(results$binomial.all.higher))
  results$binomial.bias.p <- as.numeric(as.character(results$binomial.bias.p))
  results$binomial.bias.lower <- as.numeric(as.character(results$binomial.bias.lower))
  results$binomial.bias.higher <- as.numeric(as.character(results$binomial.bias.higher))
  results$binomial2.bias.p <- as.numeric(as.character(results$binomial2.bias.p))
  results$binomial2.bias.lower <- as.numeric(as.character(results$binomial2.bias.lower))
  results$binomial2.bias.higher <- as.numeric(as.character(results$binomial2.bias.higher))
  
  results$binomial2.effectsize <- results$binomial2.bias.higher / (results$binomial2.bias.higher + 
                                                                     results$binomial2.bias.lower)
  results$binomial.effectsize <- results$binomial.bias.higher / (results$binomial.bias.higher + 
                                                                   results$binomial.bias.lower)
  
  binomial.all.effectsize1 <- results$binomial.all.higher / (results$binomial.all.higher +
                                                               results$binomial.all.lower)
  binomial.all.effectsize2 <- results$binomial.all.lower / (results$binomial.all.higher + 
                                                              results$binomial.all.lower)
  
  results$binomial.all.effectsize <- apply(data.frame(binomial.all.effectsize1, binomial.all.effectsize2), 1, max)
  
  return(results)
}

