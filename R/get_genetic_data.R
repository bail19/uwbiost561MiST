#' Generate Genetic Data
#'
#' This function generates genetic data for a given number of samples.
#' It simulates binary outcome variables, confounders, and genotypes of variants based on the specified parameters.
#'
#' @param n Number of samples to generate data for.
#' @param prob Probability for generating binary confounders.
#' @param n_unif Number of uniform random variables to generate for minor allele frequency (MAF).
#' @param min  Minimum value for the uniform distribution used in generating MAF.
#' @param max Maximum value for the uniform distribution used in generating MAF.
#' @param prob_X Probability for the effect of confounders on the outcome.
#' @param n_norm Number of normal random variables to generate for the genetic effect.
#' @param mean Mean value for the normal distribution used in generating the genetic effect.
#' @param sd Sd for the normal distribution used in generating the genetic effect.
#'
#' @return A data frame containing three variables:
#' - Y: binary outcome variable. a vector of length n.
#' - X: d confounders, either a nx1 vector or nxd matrix if d>1.
#' - G: genotypes of varaints. a n*p matrix.
#' @export
#'
#' @examples
#' # To generate genetic data for 2000 samples:
#'
#' set.seed(1234)
#' result <- get_genetic_data(n = 2000, n_unif=10, min=0.001,max=0.01, n_norm=10)
#' head(result)
get_genetic_data <- function(n, prob=0.5,
                                  n_unif, min, max,
                                  prob_X=0.5,
                                  n_norm, mean=0, sd=0.5){
  n = n
  X = stats::rbinom(n,size=1,prob=prob)
  MAF = stats::runif(n_unif,min=min,max=max)
  G = sapply(MAF,function(maf) stats::rbinom(n,size=1,prob=maf))
  eta = X*prob_X + G%*%stats::rnorm(n_norm,mean=mean,sd=sd)
  Y = stats::rbinom(n,size=1,prob=exp(eta)/(1+exp(eta)))

  return(data.frame(Y=Y, X=X, G=G))
}
