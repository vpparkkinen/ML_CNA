# Set working directory 
# If not in RStudio, assume R runs in
# a shell. Otherwise assume RStudio.
if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) 
} else {                               
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
getwd() # Check that you know where you are

library(cna) # load cna

# load some helper functions
source("funcs_datgen.R")

# Create random binary data from which to select
# observations, `Nsets` is number of 
# data sets to create (always keep at 1)
# `N` is number of observations, `varnum` is number of variables.
# Worth considering increasing `N`. Increasing `varnum` results
# in more irrelevant factors later on.
base <- bs_dat_create(Nsets = 1, N = 3000, varnum = 30)[[1]]

# Set variable names
names(base) <- c(LETTERS, sapply(1:(30-length(LETTERS)), 
                                 \(x) paste0("U", x)))

# Have a look, if you want
# head(base)

# Create 1000 random binary CNA models for "A",
# to be used as targets. Value of `x` in the `randomAsf()`
# call determines the maximum number of literals in
# the left hand side of the models. There may be
# less than `x` literals in the lhs of a model.
# Do not increase `x` much past 10-15
# to avoid your computer catching fire.
targets <- replicate(1000, randomAsf(x = 10, outcome = "A"))

# store the targets to file
writeLines(unlist(targets), file("targets.txt"))

# Corresponding to each target, create a "clean" data set where 
# each observation is some configuration of variable values
# that conforms to the target, selected from `base`.
# Note that the irrelevant factors, which are many,
# may take any values whatsoever.
cleandats <- lapply(targets, \(x) ct2df(selectCases(x, base)))

# from each "clean" data set, create
# a noisy data set by flipping the outcome value
# in about 10% of observations
ndat <- lapply(cleandats, 
               \(x) flipout(x, outcome = "A", proportion = 0.1))
               
# save data sets to 'data' folder
for(i in seq_along(ndat)){
  write.csv2(ndat[[i]], 
             file = paste0("data/dat", i, ".csv"),
             row.names = FALSE)
}
  


