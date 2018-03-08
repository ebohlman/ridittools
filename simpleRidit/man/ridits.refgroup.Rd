\name{ridits.refgroup}
\alias{ridits.refgroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility to determine reference group
%%  ~~function to do ... ~~
}
\description{
For internal use.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ridits.refgroup(x, margin, ref = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{margin}{
%%     ~~Describe \code{margin} here~~
}
  \item{ref}{
%%     ~~Describe \code{ref} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
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
function (x, margin, ref = NULL)
{
    if (length(ref) > 1) {
        refgroup <- ref
    }
    else if (length(ref) == 1) {
        if (margin == 1) {
            refgroup <- x[ref, ]
        }
        else {
            refgroup <- x[, ref]
        }
    }
    else {
        refgroup <- apply(x, 3 - margin, sum)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
