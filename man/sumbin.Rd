% Generated by roxygen2 (4.0.1): do not edit by hand
\name{sumbin}
\alias{dsumbin}
\alias{psumbin}
\alias{qsumbin}
\alias{rsumbin}
\alias{sumbin}
\title{The Distribution of a Sum of Binomials}
\usage{
dsumbin(x, size, prob)

rsumbin(n, size, prob)

psumbin(q, size, prob)

qsumbin(p, size, prob)
}
\arguments{
\item{x}{vector at which to evaluate the density}

\item{q}{vector of quantiles}

\item{p}{vector of probabilities}

\item{n}{number of observations}

\item{size}{vector of number of trials (zero or more)}

\item{prob}{vector of probability of success on each trial}
}
\value{
\code{dsumbin} gives the density, \code{psumbin} gives the distribution
function, \code{qsumbin} gives the quantile function, and \code{rsumbin}
generates random deviates.
}
\description{
Density, distribution function, quantile function, and random generation
for the sum of binomial distributed random variables with parameters
\code{size} and \code{prob}.

This may be interpreted as the number of total 'successes' in each
set of \code{size} trials with potentially differing success
probabilities and number of trials.
}
\details{
Butler and Stephens (1993) present a dynamic programming algorithm
for calculating the individual binomial density functions required
as input to compute the convolutions that are required.
However, for this version, the R function \code{dbinom}
is used instead.   Results are identical to rounding.

\code{dsumbin} is implemented first.

\code{psumbin} is implemented by calling \code{dsumbin} to obtain the
density function, and then computing the distribution function.

\code{qsumbin} is implemented by calling \code{psumbin} to obtain the
distribution function, then searching for the desired quantiles.

\code{rsumbin} was implemented simply using the R function \code{rbinom}
as needed and summing the results directly.

\code{}
}
\examples{
# Example 1 (Table 2) of Butler and Stephens (1993)
psumbin(1:7, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))

# Example 2 (Table 3) of Butler and Stephens (1993)
psumbin(
  c(275,283,291,296,300,305,311,315,320,326),
  size=c(50,100,150,200,250),
  prob=c(0.1,0.2,0.3,0.4,0.5)
)

# Example 3 (Table 4) of Butler and Stephens (1993)
psumbin(
  c(10,12,14,15,16,17,19,21,23,25),
  size=c(100,100,100,100,100),
  prob=c(0.010,0.015,0.020,0.025,0.030)
)

# Example 4 (Table 5) of Butler and Stephens (1993)
# (Note that p_3 is rounded in the original text to 0.0033)
psumbin(
  5:16,
  size=c(500,400,300,200,100),
  prob=c(0.0020, 0.0025, 1/300, 0.0050, 0.0100)
)

# Simple examples
dsumbin(0:25, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
rsumbin(30, size=c(10,20,30), prob=c(0.1,0.7,0.1))
psumbin(0:25, size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
qsumbin(c(0.2,0.5,0.9,0.99,0.999,0.9999,1.0), size=c(5,5,5,5,5), prob=c(0.02,0.04,0.06,0.08,0.10))
}
\references{
Butler, K. and Stephens, M. (1993) The distribution of a sum of
binomial random variables.  \emph{Technical Report No. 467}.
\url{http://www.dtic.mil/dtic/tr/fulltext/u2/a266969.pdf}
}

