if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
source("../funcs_datgen.R")

library(QCA)
library(cna)
library(dplyr)
library(cna)
library(randomForest)
library(caret)
library(data.table)
library(parallel)
library(tidypredict)
library(inTrees)
nf <- 3
sdat <- setNames(replicate(nf, rnorm(6000), simplify = F), 
                 LETTERS[1:nf]) |> as.data.frame()

qs <- lapply(sdat, quantile)



sdat$OUT <- case_when(sdat$A > qs$A[3] ~ as.factor(1L),
                    sdat$B > qs$B[3] ~ as.factor(1L),
                    sdat$B < qs$B[3] & sdat$C > qs$C[3] ~ as.factor(1L),
                    #sdat$A < qs$A[2] & sdat$C > qs$C[3]~ as.factor(1L),
                    .default = as.factor(0L)
                    )

noise <- cbind(as.data.frame(setNames(replicate(nf, rnorm(600), simplify = F), 
                  LETTERS[1:nf])), data.frame(OUT=as.factor(rbinom(600, 1, .5))))

sdat <- rbind(sdat, noise)

rf <- randomForest(OUT ~ ., data = sdat, 
                   importance = T, 
                   keep.forest = T,
                   keep.inbag = T,
                   ntree = 20)
rules <- grab_trees_ns(rf)

srls <- lapply(rules, \(x) unlist(strsplit(x, "\\*")))
srsl <- lapply(srls, \(x) gsub(">=|<=|<|>", "", x))

splitpoints <- vector("list", nf)
names(splitpoints) <- LETTERS[1:nf]
for(i in LETTERS[1:nf]){
  splitpoints[[i]] <- lapply(srsl,
                           \(x) x[
                             vapply(x, \(y)
                                    grepl(paste0("^", i), y),
                                    FUN.VALUE = TRUE)
                           ])
}

splitpoints <- lapply(
  splitpoints, \(x) unlist(x[sapply(x, \(y) length(y) > 0)])
)

splitpoints <- lapply(
  splitpoints, \(x) as.numeric(gsub("^[A-Z]", "", x))
)

stables <- lapply(splitpoints, table)
stables <- lapply(stables, \(x) x[order(x, decreasing = T)])
lapply(stables, head)
frsplits <- lapply(stables, \(x) x[which(x == max(x))])
frsplits <- as.numeric(sapply(frsplits, names))

unlist(lapply(qs, `[`, 3)) - frsplits



