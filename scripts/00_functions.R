
text_clarifier <- function(vec) {
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


bootstrap.binomial.bias.test <- function(df, replicates){ # recives the full dataframe and the number of replications. 

  lower.limits <- c(0.03, 0.04) # limits of p-values to run the test.
  len <- length(lower.limits) * replicates # Variable to set the number of rows of the results dataframe. 
  results <- data.frame("lower.limit" = rep(lower.limits, each = replicates), "binomial.bias.lower"=numeric(len), "binomial.bias.higher"=numeric(len), "binomial.bias.p"=numeric(len))
  for(i in 1:2){ # Interate over the two lower limits 0.03, 0.04.
    for(j in 1:replicates) { # Iterate over the number of replications
      #focal.data <- one.random.pvalue.per.paper(df) # Creates a dataframe with only on p-value per paper. 
      results[(j + replicates*(i-1)), ] <- c(lower.limits[i], binomial.bias.test(df$p.value, lower.limit= lower.limits[i], run.test=F))
    }
  }
  
  boot.results <- data.frame(lower.limit = lower.limits, binomial.bias.lower = numeric(2), binomial.bias.higher = numeric(2), binomial.bias.p = numeric(2))
  
  for(i in 2:3) boot.results[,i] <- as.numeric(tapply(results[,i], results$lower.limit, mean))
  for(i in 1:2) boot.results[i,4] <- binom.test(c(round(boot.results$binomial.bias.higher[i]), round(boot.results$binomial.bias.lower[i])))$p.value 
  
  return(boot.results)
}


binomial.bias.test <- function(p, lower.limit=0.03, run.test=T) {
  
  if(lower.limit==0.03){
    limits = c(0.03, 0.05)
    midpoint <- 0.04
    #limits.check(limits)
    #p <- p.check(p, limits)
    lower <- length(which(p<midpoint & p>=limits[1]))
    higher <- length(which(p<limits[2] & p>=midpoint)) 
  } else if(lower.limit==0.04){
    limits = c(0.04, 0.05)
    midpoint <- 0.045
    #limits.check(limits)
    #p <- p.check(p, limits)		
    lower <- length(which(p<midpoint & p>limits[1]))
    higher <- length(which(p<limits[2] & p>midpoint))
  } else{
    stop("lower.limit must be set to either 0.03 or 0.04")
  }
  
  if(run.test == T) {r <- binom.test(c(higher,lower), alternative = "greater") 
  d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
  }
  
  else d <- data.frame("lower"=lower, "higher"=higher, "p"=NA) # If we are going to call this function in a bootstrap test, we don't need to actually run the binomial test. Just count up the numbers in the bins, and return those plus a space for the p value.
  
  return(d)
}




