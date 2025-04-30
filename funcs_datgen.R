bs_dat_create <- function(Nsets = 1e3,
                          N = 30,
                          varnum = 7,
                          type = c("cs", "fs"),
                          varnames = LETTERS[1:varnum]){
  type = match.arg(type)
  if (type == "fs"){
    # c <- quote(runif(N, min = 0, max = 1))
    c <- "runif"
    args <- list(min = 0L, max = 1L)
  }
  if (type == "cs"){
    # c <- quote(rbinom(n = N, size = 1, prob = 0.5))
    c <- "rbinom"
    args <- list(size = 1, prob = 0.5)
  }
  dsets <- vector("list", Nsets)
  for(i in 1:Nsets){

    # dsets[[i]] <- data.frame(setNames(
    #   replicate(varnum, eval(c), simplify = FALSE), varnames))
    #
    ss <- if(length(N) == 1L) list(n = N) else list(n = sample(N, 1))

    dsets[[i]] <- data.frame(setNames(
      replicate(varnum,
                do.call(c, c(ss, args)), simplify = FALSE), varnames))
  }
  return(dsets)
}


#' Train/test splits
#'
#' @param x data.frame
#' @param split split size
#'
#' @returns list
#' @export
#'
#' @examples
ttsplit <- function(x, split){
  ind <- sample(2, nrow(x), 
                replace = TRUE, 
                prob = split)
  train <- x[ind==1,]
  test <- x[ind==2,]
  return(list(train=train, test=test))
}




#' Change outcome column values.
#'
#' Create a noisy data set by changing outcome column (factor) values for a
#' proportion of rows of a data set.
#'
#' @param data A `data.frame`
#' @param outcome Character; outcome column (factor) name.
#' @param proportion A numeric that determines a proportion of rows in `data`
#'   for which outcome is to be changed.
#'
#' @return A `data.frame`
#' @export
#'
#' @details Given a data frame `data` and the name of an outcome variable/factor
#'   `outcome`, `flipout()` changes the outcome value for a proportion of rows
#'   in `data`, the number of which is geiven by `proportion`. If `proportion`
#'   is not a (vulgar) fraction of `nrow(data)`, number of rows to be
#'   manipulated is determined as `round(proportion * nrow(data))`.
#'
#' @examples
flipout <- function(data, outcome, proportion) {
  N <- nrow(data)
  out_col <- which(names(data) == outcome)
  range_o <- min(data[, out_col]):max(data[, out_col])
  n_to_flip <- round(N * proportion)
  n_to_flip <- if (n_to_flip == 0L) 1L else n_to_flip
  w_rows <- sample(1:N, n_to_flip)
  for(row in w_rows){
    ov <- data[row, out_col]
    not_ov <- range_o[range_o != ov]
    change_ov_to <- sample(not_ov, 1)
    data[row, out_col] <- change_ov_to
  }
  return(data)
}

#' Make a random `asf` from a vector of factor names
#'
#' @param chr Character vector of factor names
#' @param outcome Outcome
#' @param max.conj Integer, maximum number of conjuncts in a disjunct
#' @param neg.prob Probability of negating a literal
#'
#' @return
#' @export
#'
#' @examples
rasf_hack <- function(chr, outcome, max.conj = 3L, neg.prob = 0.5){
  l <-  vector("list", length(chr))
  index <- 0L
  while(length(chr) > 0L){
    index <- index + 1L
    pick <- sample(1:max.conj, 1)
    chosen <- if(length(chr) > pick){
      sample(1:length(chr), pick)
      } else {1:length(chr)}
    l[[index]] <- chr[chosen]
    chr <- chr[-chosen]
  }
  l <- l[unlist(lapply(l, \(x) !is.null(x)))]
  out <- lapply(l,
                \(z) sapply(z,
                            \(x) ifelse(runif(1) < neg.prob, tolower(x), x),
                            USE.NAMES = FALSE))
  out <- lapply(out, \(x) paste0(x, collapse = "*"))
  out <- paste0(unlist(out), collapse = "+")
  out <- paste0(out, "<->", outcome)
  return(out)
}



#' Make a random `asf` from `data.frame` column names.
#'
#' @param data A `data.frame` -like object
#' @param outcome Outcome factor
#' @param max.conj Integer, maximum number of conjuncts
#' @param neg.prob Probability of negating a literal
#'
#' @return Character
#' @export
#' @details
#' Give `rasf_from_df` a data frame and the name of an outcome
#' and it will return a random `asf` for the outcome,
#' constructed from the column names of the data set.
#' Control maximum number of conjuncts per disjunct with `max.conj`,
#' and probability of negating a factor with `neg.prob`.
#'
#' @examples
rasf_from_df <- function(data, outcome, max.conj = 3L, neg.prob = 0.5){
  chr <- names(data[,-which(names(data) == outcome)])
  out <- rasf_hack(chr = chr,
                   outcome = outcome,
                   max.conj = max.conj,
                   neg.prob = neg.prob)
  return(out)
}




#' Correct outcome imbalances
#'
#' @param mod cna model
#' @param data data.frame with overprevalent outcome
#' @param out outcome
#' @param preval prevalence
#'
#' @returns data frame
#' @export
#' 
#' @details
#' Corrects outcome imbalance by oversampling cases with outcome absent.
#' @examples
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

grab_lits <- function(model){
  d <- frscore:::decompose_model(model)
  out <- lapply(d$lhss, \(x) unlist(strsplit(x, "\\+")))
  out <- lapply(out, \(x) unlist(strsplit(x, "\\*")))
  #names(out) <- d$rhss
  return(unlist(out))
}

fcorrect <- function(x, y){
  if(nchar(x)==0){
    return(FALSE)
  } else {
    return(frscore:::fsubmodel_asf(x, y))
  }
}

rtree2suff <- function(model){
  pattern_neg <- "([A-Za-z0-9]*)is0"
  pattern_pos <- "([A-Za-z0-9]*)is1"
  matches <- gregexpr(pattern_neg, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) tolower(gsub("is0", "", x))
  ) 
  matches <- gregexpr(pattern_pos, model)
  regmatches(model, matches) <- lapply(
    regmatches(model, matches), \(x) toupper(gsub("is1", "", x))
  )
  return(model)
}


rp_rules_to_cna <- function(model, outcome, cutoff = 0.7){
  rules <- rpart.rules(model)
  rules[, outcome] <- as.numeric(rules[, outcome])
  rules <- rules[rules[, outcome] > cutoff,]
  rules <- do.call(paste, rules[,3:length(rules)])
  rules <- sapply(rules, \(x) gsub(" *$", "", x), USE.NAMES = FALSE)
  rules <- sapply(rules, \(x) gsub(" ", "", x), USE.NAMES = FALSE)
  rules <- sapply(rules, \(x) gsub("&", "*", x), USE.NAMES = FALSE)
  suffs <- sapply(rules, rtree2suff, USE.NAMES = FALSE)
  if (nchar(paste0(suffs, collapse = "")) == 0) {model <-  ""} else {
    model <- paste(suffs, collapse = "+")
    model <- paste0(model, "<->", outcome)
  }
  return(model)
}


