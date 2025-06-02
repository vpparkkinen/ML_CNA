if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}

library(cna)
library(data.table)
library(frscore)
library(pre)
source("../funcs_datgen.R")

varnum <- 18
outcome <- "A"
base <- as.data.table(bs_dat_create(Nsets = 1, N = 1800, varnum = varnum)[[1]])


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
targets <- replicate(100, rasf_hack(LETTERS[1:relevants], 
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


pre2suff <- function(model){
  pattern_neg <- "([A-Za-z0-9]*)<=0"
  pattern_pos <- "([A-Za-z0-9]*)>0"
  matches <- gregexpr(pattern_neg, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) tolower(gsub("<=0", "", x))
  ) 
  matches <- gregexpr(pattern_pos, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) toupper(gsub(">0", "", x))
  )
  return(model)
}



pre2cna <- function(formula, data, ...){
  dots <- list(...)
  fcall <- c(list(formula=formula, data=data), dots)
  dre <- do.call(pre, fcall)
  rls <- coef(dre)
  rls <- rls[startsWith(rls$rule, "rule"),]
  rls <- rls[rls$coefficient > 0,3]
  rls <- sapply(rls, \(x) gsub("&","*", noblanks(x)), simplify = FALSE)
  rls <- sapply(rls, pre2suff, simplify = FALSE)
  sf <- paste(unlist(rls), collapse = "+")
  #sfc <- paste0(sf, "<->E")
  if(nchar(sf)==0) return("") else return(rreduce(sf))
}


respre <- lapply(ndat, \(x) pre2cna(formula = A~., data = x, ntrees = 180))

totcor <- mapply(fcorrect, respre, targets, SIMPLIFY = F)
unlist(totcor) |> sum()


# da <- ct2df(selectCases("A+B*C<->E"))
# 
# dr <- da
# dr$E <- as.factor(da$E)
# #dr <- as.data.frame(lapply(dr,as.factor))
# #dr <- d.error
# dr <- replicate(500, dr, simplify = F)
# dr <- rbindlist(dr)
# dre <- pre(E~., data = dr, family = "binomial", ntrees = 500)
# rls <- coef(dre)
# #pmatch("rule", "A")
# rls <- rls[startsWith(rls$rule, "rule"),]
# rls <- rls[rls$coefficient > 0,3]



