---
title: "Final Project""
author: |
  | Camacho Jonathan
  | Computing for the Social Sciences
  | University of Chicago
---

This homework contains two part:

- An extraction of p-values from a data set. 
- Conducted an exploratory analysis of the data set. 
- Performed a binomial test for all articles sampled and by categories. 
- Rendered the results as a web site at 

## Usefull sites
In this final project the following pages were of particular help:

- A tutorial for Xpath language: http://www.w3schools.com/xml/xpath_intro.asp 
- Tutorial for tm R package: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
- Tutorial for the R package XML: https://cran.r-project.org/web/packages/XML/XML.pdf
- Site that evaluate regex expressions:https://regexper.com/#%5B%5Ea-zA-Z0-9*%23†_%5D%5BPp%5D%5Bs%5D*%5B%3D%3C%3E≤≥%5D%5B%5C%5Cs%5D*%5B01%5D%3F%5B.%5D%5B%5C%5Cd%5D

- Information about binomial tests: https://www.calvin.edu/~scofield/courses/m343/F15/handouts/binomialTests.pdf

In the folder fp-template you will find the following scripts in the folder scripts:

- 00_functions.R: File with helper functions some of them adapted from Head, M. et al., (2015)
- 01_sample_files.R: Randomly sample files form the full PubMed corpus. Deactivated because the full corpus was not uploaded to Github. 
- 02_extract_p-values.R: Main function that extract p-values from nXML files along with other variables. 
- 03_tidy_data.R: Performs routine Transformations and tidy operations to data sets.
- 04_analysis.R: Performs exploratory and binomial analysis.  
- runfile.R: Runs the whole analysis.  

Data sets:

- In "data/raw" directory p-values_df.csv: This is the resulting data set from the scripts 02_extract_p-values.R. It is a demonstration of the capabilities of the script. The actual analysis was conducted using the data set p_values_df_full_set.csv. This data set was built sampling 100,000 files and then extracting the p-values in advance.The whole process took 17 hours. That is the reason the analysis was conducted with this data set.

- Sampled_corpus: Example of sampled files by the script 01_sample_files.R.

In order to re-run the report you need to fork and clone the repository and then execute runfile.R.

### Citations 

Head, M. L., Holman, L., Lanfear, R., Kahn, A. T., & Jennions, M. D. (2015). The Extent and Consequences of P-Hacking in Science. PLOS Biology, 13(3), e1002106–15. http://doi.org/10.1371/journal.pbio.1002106

