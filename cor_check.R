if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
library(cna)
outcome = "A"
# load targets
targets <- readLines(file("targets.txt"))
# load preprocessed results created by DT/TM/whatever
conds <- readLines(file("[YOUR_SAVED_RESULTS_HERE].txt"))

# do this below if the the stuff in `conds` is not in DNF
#conds <- unlist(lapply(conds, \(x) getCond(selectCases(x))))

nemp <- sapply(conds, \(x) nchar(x)>0) # non-empties

# minimize, check for non-empty result again, for some reason
r_conds <- lapply(conds[nemp], 
                  \(x) if (nchar(x) == 0) return("") else rreduce(x)) 

# paste the outcome to model lhs's
r_conds <- lapply(r_conds, \(x) sapply(x, \(y) paste0(y, "<->", outcome),
                                       USE.NAMES = FALSE))

## Check correctness, use the command on line 31 (commented out) 
## if using ereduce() for minimization. 
## With rreduce() there's only one minimized model for 
## each redundant one.

#cors <- mapply(\(x, y) sapply(x, \(z) is.submodel(z, y)),
#          x = r_conds,
#          y = targets[nemp],
#          SIMPLIFY = FALSE,
#          USE.NAMES = TRUE)

cors <- mapply(\(x, y) is.submodel(x, y),
               x = r_conds,
               y = targets[nemp],
               SIMPLIFY = FALSE,
               USE.NAMES = TRUE)

tstamp <- gsub(" ", "_", date())

# save correctness check result
saveRDS(cors, paste0("TM_checked", tstamp)) 

cors <- unlist(lapply(cors, any))
cor_percentage <- sum(cors) / length(targets)


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

c_in_tar <- mapply(\(x,y) sapply(x, \(z) grepl(z, y)), x=model_facs, y=targets, SIMPLIFY=FALSE)

which(sapply(c_in_tar, all)) |> length()






