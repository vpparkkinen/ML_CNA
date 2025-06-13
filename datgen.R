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
getwd() # Are we where we should be?
library(cna) 
library(data.table)
# load some helper functions
source("funcs_datgen.R")

# Create random binary data from which to select
# observations, `Nsets` is number of 
# data sets to create (always keep at 1)
# `N` is number of observations, `varnum` is number of variables.
# Worth considering increasing `N`. Increasing `varnum` results
# in more irrelevant factors later on.
varnum <- 18 # n. of variables in total
relevants <- 15 # n. of causally relevant variables
n_sets <- 100 # n. of targets and data sets to create
outcome <- "A" # outcome

# random data to choose observations from 
base <- as.data.table(bs_dat_create(Nsets = 1, N = 100000, varnum = varnum)[[1]])


# Set variable names past LETTERS to 'U1', 'U2' ...if more vars than length(LETTERS)
if (length(LETTERS) < varnum){
  names(base) <- c(LETTERS, sapply(1:(varnum-length(LETTERS)), 
                                   \(x) paste0("U", x)))
}

fnames <- names(base)
exo_names_potential <- fnames[-which(fnames==outcome)]
exof_names <- exo_names_potential[1:relevants]
# Create random binary CNA models for "A",
# to be used as targets. 
# `relevants`:=number of factors included in the models.
targets <- replicate(n_sets, rasf_hack(exof_names, 
                                   outcome = outcome,
                                   max.conj = 6,
                                   neg.prob = 0.5))

# check that outcome does not accidentally appear in the model lhss

lhss <- sapply(targets, lhs)
if(any(unlist(sapply(lhss, \(x) grepl(outcome, x, ignore.case = T))))){
  stop("CHECK OUTCOME!!!!!")
}

#cleandats <- lapply(targets, \(x) ct2df(selectCases(x, base)))

# Noise-free data sets w/ outcome prevalence =<80%
# This will take a while
cleandats <- lapply(targets, \(x) alt_SC(x, base, outcome, preval = 0.8))

# Check that outcome prevalence is not zero and discard
# data set / target if that is the case
check_vars <- function(x, outcome){
  vars <- unlist(lapply(x, \(y) var(y[,outcome])))
  return(which(vars > 0))
}

keep <- check_vars(cleandats, outcome = outcome)
targets <- targets[keep]
cleandats <- cleandats[keep]


# From each noise-free data set, create
# a noisy data set by flipping the outcome value
# in about 10% of observations.
# Will take a while.
ndat <- lapply(cleandats, 
               \(x) flipout(x, outcome = outcome, proportion = 0.1))
               
# check that outcome varies in each noisy data set, discard if not

keep2 <- check_vars(ndat, outcome)
targets <- targets[keep2]
ndat <- ndat[keep2]

# store the targets to file
writeLines(unlist(targets), file("targets.txt"))

# save data sets to 'data' folder
for(i in seq_along(ndat)){
  fwrite(ndat[[i]], 
             file = paste0("data/dat", i, ".csv"),
             row.names = FALSE)
}
  


