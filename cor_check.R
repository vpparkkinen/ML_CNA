if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
library(cna)
library(data.table)
library(parallel)
outcome = "A"
# load targets
targets <- readLines(file("targets.txt"))
# load preprocessed results created by DT/TM/whatever

res <- fread("tmres30062025-13h43m.csv", sep = ",")
res[,V1:=NULL]
#dat1 <- fread("data/dat1.csv", sep = ",")
  

iters <- nrow(unique(res[,-c("models")]))
targets <- replicate(iters, targets)
targets <- c(targets)


# do this below if the the stuff in `conds` is not in DNF
#conds <- unlist(lapply(conds, \(x) getCond(selectCases(x))))

nemp <- sapply(res$models, \(x) nchar(x)>0) # non-empties

targets <- targets[nemp]
res <- res[nemp,]


# minimize, check for non-empty result again, for some reason
r_conds <- mclapply(res$models,
                  \(x) if (nchar(x) == 0) return("") else rreduce(x))
r_conds_backup <- r_conds

tautologies <- sapply(r_conds, \(x) x == "1")
avg_tautologies <- sum(tautologies) / length(tautologies)
# paste the outcome to model lhs's
r_conds <- lapply(r_conds, \(x) sapply(x, \(y) paste0(y, "<->", outcome),
                                       USE.NAMES = FALSE))

r_conds <- sapply(r_conds, \(x) gsub("^1", "TAUT", x))
res[,rmodels := r_conds]

## Check correctness, use the command on line 31 (commented out)
## if using ereduce() for minimization.
## With rreduce() there's only one minimized model for
## each redundant one.

#cors <- mapply(\(x, y) sapply(x, \(z) is.submodel(z, y)),
#          x = r_conds,
#          y = targets[nemp],
#          SIMPLIFY = FALSE,
#          USE.NAMES = TRUE)

cors <- mcmapply(\(x, y) frscore:::fsubmodel_asf(x, y),
               x = r_conds,
               y = targets,
               SIMPLIFY = FALSE,
               USE.NAMES = TRUE)

res[,correct:=cors]
#res[,sum(correct), by = c("number_of_clauses", "T", "s")]

tstamp <- gsub(" ", "_", date())

# save correctness check result
saveRDS(res, paste0("TM_checked", tstamp))

cors <- unlist(lapply(cors, any))
cor_percentage <- sum(cors) / length(targets)

res[correct==TRUE, sapply(rmodels, getComplexity)] |> mean()
### Optional: Check for determining if the models include
### irrelevant factors.
grab_lits <- function(model){
  d <- frscore:::decompose_model(model)
  out <- lapply(d$lhss, \(x) unlist(strsplit(x, "\\+")))
  out <- lapply(out, \(x) unlist(strsplit(x, "\\*")))
  #names(out) <- d$rhss
  return(unlist(out))
}

model_facs <- lapply(r_conds, grab_lits)

c_in_tar <- mapply(\(x,y) sapply(x, \(z) grepl(z, y, ignore.case = T)), x=model_facs, y=targets, SIMPLIFY=FALSE)

which(sapply(c_in_tar, all)) |> length()
