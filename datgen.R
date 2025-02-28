if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}

library(cnasimtools)

a <- replicate(2, randomDat(6, outcome = "A"), simplify = FALSE)

targets <- lapply(a, \(x) attributes(x)$target)
writeLines(unlist(targets), file("targets.txt"))


ndat <- mapply(prevalence_compliant_noisify,
               model = targets, 
               data = a,
               MoreArgs = list(outcome = "A", noiselevel = 0.125),
               SIMPLIFY = FALSE)





for(i in seq_along(ndat)){
  write.csv2(ndat[[i]], 
             file = paste0("data/dat", i, ".csv"),
             row.names = FALSE)
}



