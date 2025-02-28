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
re <- readLines(file("ress.txt"))

conds <- unlist(lapply(re, \(x) getCond(selectCases(x))))

r_conds <- lapply(conds, ereduce) 
r_conds <- lapply(r_conds, \(x) sapply(x, \(y) paste0(y, "<->", outcome)))

cors <- mapply(\(x, y) sapply(x, \(z) is.submodel(z, y)), 
          x = r_conds, 
          y = targets,
          SIMPLIFY = FALSE,
          USE.NAMES = FALSE)

cors <- unlist(lapply(cors, any))

cor_percentage <- sum(cors) / length(targets)
