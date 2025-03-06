if(is.na(Sys.getenv("RSTUDIO", unset = NA))){
  setwd(system2("pwd", stdout = TRUE)) # if not in RStudio, assume R runs in
} else {                               # a shell. otherwise assume RStudio
  path <- rstudioapi::getActiveDocumentContext()$path
  Encoding(path) <- "UTF-8"
  setwd(dirname(path))
}

library(cnasimtools)

#a <- replicate(1, randomDat(4, outcome = "A"), simplify = FALSE)



base <- bs_dat_create(1, N = 3000, varnum = 30)[[1]]

names(base) <- c(LETTERS, sapply(1:(30-length(LETTERS)), \(x) paste0("U", x)))

targets <- replicate(10, randomAsf(10, outcome = "A"))

cleandats <- lapply(targets, \(x) ct2df(selectCases(x, base)))

#targets <- lapply(a, \(x) attributes(x)$target)
writeLines(unlist(targets), file("targets.txt"))


# ndat <- mapply(prevalence_compliant_noisify,
#                model = targets, 
#                data = cleandats,
#                MoreArgs = list(outcome = "A", noiselevel = 0.125),
#                SIMPLIFY = FALSE)

ndat <- lapply(cleandats, \(x) flipout(x, outcome = "A", proportion = 0.1))
               

for(i in seq_along(ndat)){
  write.csv2(ndat[[i]], 
             file = paste0("data/dat", i, ".csv"),
             row.names = FALSE)
}
  


