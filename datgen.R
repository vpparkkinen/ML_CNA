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
library(data.table)
# load some helper functions
source("funcs_datgen.R")

# Create random binary data from which to select
# observations, `Nsets` is number of 
# data sets to create (always keep at 1)
# `N` is number of observations, `varnum` is number of variables.
# Worth considering increasing `N`. Increasing `varnum` results
# in more irrelevant factors later on.
varnum <- 18
outcome <- "A"
base <- as.data.table(bs_dat_create(Nsets = 1, N = 100000, varnum = varnum)[[1]])


# Set variable names if more vars than length(LETTERS)
if (length(LETTERS) < varnum){
  names(base) <- c(LETTERS, sapply(1:(varnum-length(LETTERS)), 
                                   \(x) paste0("U", x)))
}

# Have a look, if you want
# head(base)

# Create random binary CNA models for "A",
# to be used as targets. 
# `relevants`:=number of factors included in the models.
relevants <- 15
targets <- replicate(20, rasf_hack(LETTERS[1:relevants], 
                                   outcome = "A",
                                   max.conj = 6,
                                   neg.prob = 0.5))




#cleandats <- lapply(targets, \(x) ct2df(selectCases(x, base)))

# Noise-free data sets w/ outcome prevalence =<80%
cleandats <- lapply(targets, \(x) alt_SC(x, base, outcome,0.8))

# Check that outcome prevalence not zero and discard
# data set / target if that is the case
check_vars <- function(x){
  vars <- unlist(lapply(x, \(y) var(y[,outcome])))
  return(which(vars > 0))
}

keep <- check_vars(cleandats)
targets <- targets[keep]
cleandats <- cleandats[keep]

# Calculate outcome prevalences and discard data sets
# where outcome prevalence >90%.
# Ns <- unlist(lapply(cleandats, nrow))  
# out_pres <- unlist(lapply(cleandats, \(x) sum(x[,outcome])))
# o_prevs <- out_pres / Ns
# keep <- which(o_prevs < 0.8)
# targets <- targets[keep]
# cleandats <- cleandats[keep]


# from each "clean" data set, create
# a noisy data set by flipping the outcome value
# in about 10% of observations
ndat <- lapply(cleandats, 
               \(x) flipout(x, outcome = "A", proportion = 0.1))
               
# check that outcomes vary, discard if not

keep2 <- check_vars(ndat)
targets <- targets[keep2]
ndat <- ndat[keep2]

# store the targets to file
writeLines(unlist(targets), file("targets.txt"))

# save data sets to 'data' folder
for(i in seq_along(ndat)){
  write.csv2(ndat[[i]], 
             file = paste0("data/dat", i, ".csv"),
             row.names = FALSE)
}
  


