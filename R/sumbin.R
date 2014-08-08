##' @name sumbin
##' @rdname sumbin
##'
##' @title The Distribution of a Sum of Binomials
##' 
##' @description 
##' Density, distribution function, quantile function, and random generation
##' for the sum of binomial distributed random variables with parameters
##' \code{size} and \code{prob}.
##' 
##' This may be interpreted as the number of total 'successes' in each
##' set of \code{size} trials with potentially differing success 
##' probabilities and number of trials.
##'
##' @param x vector at which to evaluate the density
##' @param q vector of quantiles
##' @param p vector of probabilities
##' @param n number of observations
##' @param size vector of number of trials (zero or more)
##' @param prob vector of probability of success on each trial
##'
##' @return 
##' \code{dsumbin} gives the density, \code{psumbin} gives the distribution
##' function, \code{qsumbin} gives the quantile function, and \code{rsumbin}
##' generates random deviates.
##' 
##' @details
##' Butler and Stephens (1993) present a dynamic programming algorithm
##' for calculating the individual binomial density functions required
##' as input to compute the convolutions that are required.  
##' However, for this version, the R function \code{dbinom}
##' is used instead.   Results are identical to rounding.
##' 
##' \code{dsumbin} is implemented first.
##' 
##' \code{psumbin} is implemented by calling \code{dsumbin} to obtain the
##' density function, and then computing the distribution function.
##' 
##' \code{qsumbin} is implemented by calling \code{psumbin} to obtain the
##' distribution function, then searching for the desired quantiles.
##' 
##' \code{rsumbin} was implemented simply using the R function \code{rbinom}
##' as needed and summing the results directly.
##' 
##' \code{}
##' 
##' @references
##' Butler, K. and Stephens, M. (1993) The distribution of a sum of 
##' binomial random variables.  \emph{Technical Report No. 467}.
##' \url{http://www.dtic.mil/dtic/tr/fulltext/u2/a266969.pdf}
##' 
##' @examples
##' # Example 1 (Table 2) of Butler and Stephens (1993)
##' psumbin(1:7, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
##' 
##' # Example 2 (Table 3) of Butler and Stephens (1993)
##' psumbin(
##'   c(275,283,291,296,300,305,311,315,320,326),
##'   size=c(50,100,150,200,250), 
##'   prob=c(0.1,0.2,0.3,0.4,0.5)
##' )
##' 
##' # Example 3 (Table 4) of Butler and Stephens (1993)
##' psumbin(
##'   c(10,12,14,15,16,17,19,21,23,25),
##'   size=c(100,100,100,100,100), 
##'   prob=c(0.010,0.015,0.020,0.025,0.030)
##' )
##'   
##' # Example 4 (Table 5) of Butler and Stephens (1993)
##' # (Note that p_3 is rounded in the original text to 0.0033)
##' psumbin(
##'   5:16,
##'   size=c(500,400,300,200,100), 
##'   prob=c(0.0020, 0.0025, 1/300, 0.0050, 0.0100)
##' )
##' 
##' # Simple examples

NULL

##' @rdname sumbin
##' @examples
##' dsumbin(0:25, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
##' @export
 
dsumbin <- function(x, size, prob) {
  
  
  U <- rep(0, sum(size)+1)        ### Set up the final result vector now
  
  Y <- dbinom(0:size[1], size[1], prob[1])
  Z <- dbinom(0:size[2], size[2], prob[2])
  
  Y <- c(Y, rep(0, size[2]))      ### Pad Y and Z appropriately
  Z <- c(Z, rep(0, size[1]))
  
  for (j in 0:(size[1]+size[2])) {
    
    U[j+1] <- sum(Y[(0+1):(j+1)] * Z[(j+1):(0+1)])
    
  } 
  
  for (k in 3:length(size)) {
    
    Y <- U
    Z <- dbinom(0:size[k], size[k], prob[k])
    
    Z <- c(Z, rep(0, sum(size[1:(k-1)]))) ### Pad Z appropriately
    
    for (j in 0:(sum(size[1:k]))) {
      
      U[j+1] <- sum(Y[(0+1):(j+1)] * Z[(j+1):(0+1)])
      
    } 
  }
  
  return(U[(x+1)])
  
}

##' @rdname sumbin
##' @examples
##' rsumbin(30, size=c(10,20,30), prob=c(0.1,0.7,0.1))
##' @export

rsumbin <- function(n, size, prob) {
  
  x <- numeric(n)
  M <- cbind(size, prob)
  
  for (i in 1:n) {
    x[i] <- sum(apply(M, 1, function(x) {rbinom(1, x[1], x[2])}))
  }
  
  return(x)
  
}

##' @rdname sumbin
##' @examples
##' psumbin(0:25, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
##' @export

psumbin <- function(q, size, prob) {
  
  # First, get the density function.
  
  U <- dsumbin(x=0:sum(size), size=size, prob=prob)
  
  # Then, calculate the cumulative distribution function
  # and obtain the desired probabilities.
  
  return(cumsum(U)[q+1])   
}

##' @rdname sumbin
##' @examples
##' qsumbin(c(0.2,0.5,0.9,0.99,0.999,0.9999,1.0), size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
##' @export

qsumbin <- function(p, size, prob) {
  
  # First, get the entire cumulative distribution function.
  
  U <- psumbin(q=0:sum(size), size=size, prob=prob)
  
  # Then, get the correct indices.  Happily, findInterval
  # seems to provide the right answer out of the box in this
  # particular situation.
  
  return(findInterval(p, U, rightmost.closed=TRUE))
  
}
