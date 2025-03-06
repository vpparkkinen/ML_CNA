if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}
library(cna)
outcome = "A"
targets <- readLines(file("targets.txt"))
conds <- readLines(file("tm_results.txt"))

# do this if the the stuff in `conds` is not in DNF
#conds <- unlist(lapply(conds, \(x) getCond(selectCases(x))))

nemp <- sapply(conds, \(x) nchar(x)>0)

r_conds <- lapply(conds[nemp], \(x) if (nchar(x) == 0) return("") else rreduce(x)) 
r_conds <- lapply(r_conds, \(x) sapply(x, \(y) paste0(y, "<->", outcome),
                                       USE.NAMES = FALSE))

cors <- mapply(\(x, y) sapply(x, \(z) is.submodel(z, y)), 
          x = r_conds, 
          y = targets[nemp],
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE)

cors <- unlist(lapply(cors, any))

cor_percentage <- sum(cors) / length(targets)
