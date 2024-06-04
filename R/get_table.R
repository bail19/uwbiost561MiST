#' Summarize MiST Analysis Results in a Table
#'
#' This function takes the result of a MiST analysis and summarizes the estimates and pvalues in a table format.
#'
#' @param res A list for the result of a MiST analysis, expected to include 'stat' and 'pvalue'.
#'
#' @return A kable of MiST analysis results.
#' @export
#'
#' @examples
#' # Data generation
#' n = 2000
#' set.seed(1234)
#' X = rbinom(n,size=1,prob=0.5)
#' MAF = runif(10,min=0.001,max=0.01)
#' G = sapply(MAF,function(maf) rbinom(n,size=1,prob=maf))
#' eta = X*0.5 + G%*%rnorm(10,mean=0,sd=0.5)
#' Y = rbinom(n,size=1,prob=exp(eta)/(1+exp(eta)))
#' d = 1
#' p = 10
#'
#' data = data.frame(Y=Y, X=X,G=G)
#' result <- MiST(data = data,
#'          outcome_type = "Binary",
#'          d = d,
#'          p = p,
#'          R = 1,
#'          weight_method = "User")
#'
#' get_table(result)
get_table <- function(res) {
  mist_stat <- res$stat
  mist_p <- res$pvalue
  tab1 <- t(data.frame(stat = mist_stat, pvalue=mist_p))
  colnames(tab1) <- c("f", "r", "oMiST", "aMiST", "fMiST")
  knitr::kable(tab1, caption = "Output of MiST Analysis")
}
