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