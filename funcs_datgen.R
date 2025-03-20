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

