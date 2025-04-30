if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
library(rpart)
#library(tree)
library(caTools)
library(cna)
library(randomForest)
library(caret)
library(data.table)
library(parallel)
library(tidypredict)
library(parsnip)
library(frscore)
library(rpart.plot)
source("../funcs_datgen.R")



# Create random binary data from which to select
# observations, `Nsets` is number of 
# data sets to create (always keep at 1)
# `N` is number of observations, `varnum` is number of variables.
# Worth considering increasing `N`. Increasing `varnum` results
# in more irrelevant factors later on.
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
targets <- replicate(50, rasf_hack(LETTERS[1:relevants], 
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

args <- expand.grid(c(2,3,4,5), c(0,0.1,0.2, 0.3))
names(args) <- c("maxdepth", "cp")

pt <- vector("list", nrow(args))

rf_i_select <- function(dat, outcome, ntree){
  te <-   te <- randomForest(x = dat[,-which(names(dat) == outcome)], 
                             y = as.factor(dat[,outcome]),
                             ntree = ntree, importance = TRUE)
  imp <- importance(te, class = 1)
  dimp <- names(imp[,4][which(imp[,4] > quantile(imp[,4], 
                                                 probs = .75))])
  dimp <- paste(dimp, collapse = "+")
  rf_f <- formula(paste0(outcome, "~", dimp))
  return(rf_f)
}

forms <- mclapply(ndat, \(x) rf_i_select(x, outcome, 180))
for(i in seq_along(pt)){
  mods <- mapply(rpart, forms, ndat, MoreArgs = list(model = TRUE, maxdepth = args$maxdepth[i], cp = args$cp[i], method = "class"), SIMPLIFY = FALSE)
  mods2 <- lapply(mods, \(x) rp_rules_to_cna(x, outcome))
  pt[[i]] <- unlist(mods2)
}

mix <- sapply(1:nrow(args), \(x) rep(x, length(ndat)), simplify = FALSE) |> unlist()

rest <- args[mix,]
rest$model <- unlist(pt)
rest$target <- replicate(nrow(args), unlist(targets), simplify = FALSE) |> unlist()
rest$correct <- mapply(fcorrect, rest$model, rest$target, SIMPLIFY = FALSE) |> unlist()
rest$complexity <- sapply(rest$model, cna::getComplexity, simplify = FALSE) |> unlist()
rest <- as.data.table(rest)


rest[, mean(correct), by = c("maxdepth", "cp")]

tstamp <- gsub(" ", "_", date())

saveRDS(rest, paste0("rf_dt", tstamp))

# rest <- as.data.table(rest)
# rest[,mean(correct), by=c("maxdepth", "cp")]
# rest[correct==TRUE]
# 
# model_facs <- lapply(rest$model, 
#                      \(x) if(nchar(x) == 0) "" else grab_lits(x))
# 
# c_in_tar <- mapply(\(x,y) 
#                    sapply(x, \(z) {
#                      if(nchar(z) == 0) FALSE else grepl(z, y)
#                    }), 
#                    x=model_facs, 
#                    y=targets, 
#                    SIMPLIFY=FALSE)
# 
# which(sapply(c_in_tar, all)) |> length()








