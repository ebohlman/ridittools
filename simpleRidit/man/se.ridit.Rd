\name{se.ridit}
\alias{se.ridit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute standard error of mean ridit for group given reference group
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
se.ridit(v, ref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
  same as mean.ridit
%%     ~~Describe \code{v} here~~
}
  \item{ref}{
  same as mean.ridit
%%     ~~Describe \code{ref} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
standard error of mean ridit
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Fleiss, p. 154
%% ~put references to the literature/web site here ~
}
\author{
Eric Bohlman
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (v, ref)
{
    N <- sum(ref)
    n <- sum(v)
    term1 <- (n + 1)/N
    term2 <- 1/(N * (N + n - 1))
    term3 <- sum((ref + v)^3)/(N * (N + n) * (N + n - 1))
    (1/(2 * sqrt(3 * n))) * sqrt(1 + term1 + term2 - term3)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
