if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
source("../funcs_datgen.R")

library(cna)
library(randomForest)
library(caret)
library(data.table)
library(parallel)
library(tidypredict)
# #targets <- unlist(replicate(5, randomAsf(5, outcome = "A"), simplify = F))
# 
# mod <- "A*b+B*a<->C"
# da <- ct2df(selectCases(mod))
# dalong <- unlist(replicate(500, list(da), simplify = F), recursive = F)
# dalong <- rbindlist(dalong)
# dalong[, `:=`("U1"=rbinom(nrow(dalong), 1, .5), 
#               "U2"=rbinom(nrow(dalong), 1, .5),
#               "U3"=rbinom(nrow(dalong), 1, .5))]
# dalong <- flipout(as.data.frame(dalong), "C", 0.1)
# cor(dalong)
# 
# ind <- sample(2, nrow(dalong), replace = TRUE, prob = c(0.8, 0.2))
# train <- dalong[ind==1,]
# test <- dalong[ind==2,]

ttsplit <- function(x, split){
  ind <- sample(2, nrow(x), 
                replace = TRUE, 
                prob = split)
  train <- x[ind==1,]
  test <- x[ind==2,]
  return(list(train=train, test=test))
}


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
targets <- replicate(40, rasf_hack(LETTERS[1:relevants], 
                                    outcome = "A",
                                    max.conj = 6,
                                    neg.prob = 0.5))


# Corresponding to each target, create a "clean" data set where 
# each observation is some configuration of variable values
# that conforms to the target, selected from `base`.
# Note that the irrelevant factors
# may take any values whatsoever.
# The way the targets are created results in ideal data
# with extreme outcome prevalence. The function below
# corrects for this by oversampling cases with
# outcome absent and forcing desired outcome prevalence.
alt_SC <- function(mod,data,out,preval){
  a <- ct2df(selectCases(mod, data))
  n <- nrow(a)
  nout <- sum(a[out])
  o_prev <- nout / n
  if(o_prev > preval){
    nn <- preval*n
    to_add <- ceiling((nout - nn) / preval)
    b <- ct2df(selectCases(mod, data[get(out)==0,]))
    neg_outs <- b[sample(1:nrow(b), to_add, replace = TRUE),]
    a <- rbind(a, neg_outs)
  }
  return(a)
} 


#cleandats <- lapply(targets, \(x) ct2df(selectCases(x, base)))

# Noise-free data sets w/ outcome prevalence =<80%
cleandats <- lapply(targets, \(x) alt_SC(x, base, outcome,0.8))

# Check that outcome prevalence not zero and discard
# data set / target if that is the case
check_vars <- function(x){
  vars <- unlist(lapply(x, \(y) var(as.numeric(y[,outcome]))))
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

ndat <- lapply(ndat, \(x){
  x[[outcome]] <- as.factor(x[[outcome]])
  return(x)})

# check that outcomes vary, discard if not

keep2 <- check_vars(ndat)
targets <- targets[keep2]
ndat <- ndat[keep2]

# ttsplits <- lapply(ndat, \(x) ttsplit(x, split = c(0.8, 0.2)))


# rf <- randomForest(x = dalong[,-which(names(dalong)=="C")], 
#                    y = as.factor(dalong$C),
#                    data = train,
#                    proximity = TRUE,
#                    importance = TRUE,
#                    ntree = 50,
#                    keep.forest = TRUE
#                    )

# rfs <- lapply(ttsplits, 
#               \(z)randomForest(x = z[,-which(names(z) == outcome)]),
#               y = as.factor(z[[outcome]]),
#               data)

# importance(rf)
# getTree(rf, k=4, labelVar = TRUE)

treetosuff <- function(model){
  pattern_leq <- "([A-Za-z0-9]*)<0\\.5"
  pattern_gt <- "([A-Za-z0-9]*)>=0\\.5"
  matches <- gregexpr(pattern_leq, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) tolower(gsub("<0\\.5", "", x))
  ) 
  matches <- gregexpr(pattern_gt, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) toupper(gsub(">=0\\.5", "", x))
  )
  return(model)
}

grab_trees <- function(rf){
  trees <- tidypredict_fit(rf)
  trees <- lapply(trees, as.character)
  trees <- lapply(trees, \(x) x[2:length(x)])
  trees <- unlist(trees)
  trees <- sapply(trees, \(x) gsub(" ", "", x), USE.NAMES = F)
  trees <- trees[grepl("~\"1\"", trees)]
  trees <- gsub("&", "*", trees)
  trees <- gsub("~\"1\"", "", trees)
  trees <- sapply(trees, treetosuff, USE.NAMES = F)
  return(trees)
}

treesuffs_to_dnf <- function(suffs){
  if(length(suffs)==0) return("")
  ta <- table(suffs)
  best <- ta[which(ta >= sd(ta)*2)]
  return(paste(names(best), collapse = "+"))
}

rf_to_dnf <- function(...){
  dots <- list(...)
  rf <- do.call(randomForest, dots)
  out <- grab_trees(rf)
  out <- treesuffs_to_dnf(out)
  return(out)
}

res <- mclapply(ndat, 
                \(z) rf_to_dnf(x = z[,-which(names(z)==outcome)],
                               y = z[,outcome],
                               maxnodes = 6,
                               ntree = 180)
              )

minim <- lapply(res, \(x){
  if(nchar(x)>0) rreduce(x) else x
}) 
  
models <- lapply(minim, 
                \(x) if(x != "1" && nchar(x) > 0) paste0(x, "<->", outcome) else
                  "")

fcorrect <- function(x, y){
  if(nchar(x)==0){
    return(FALSE)
    } else {
    return(frscore:::fsubmodel_asf(x, y))
  }
}

cor <- mcmapply(fcorrect, x = models, y = targets)
corpr <- sum(unlist(cor)) / length(cor)

grab_lits <- function(model){
  d <- frscore:::decompose_model(model)
  out <- lapply(d$lhss, \(x) unlist(strsplit(x, "\\+")))
  out <- lapply(out, \(x) unlist(strsplit(x, "\\*")))
  #names(out) <- d$rhss
  return(unlist(out))
}

model_facs <- lapply(models, 
                     \(x) if(nchar(x) == 0) "" else grab_lits(x))

c_in_tar <- mapply(\(x,y) 
                   sapply(x, \(z) {
                     if(nchar(z) == 0) FALSE else grepl(z, y)
                   }), 
                   x=model_facs, 
                   y=targets, 
                   SIMPLIFY=FALSE)

which(sapply(c_in_tar, all)) |> length()


