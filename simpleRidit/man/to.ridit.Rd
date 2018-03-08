\name{to.ridit}
\alias{to.ridit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute ridit scores for group
%%  ~~function to do ... ~~
}
\description{
Computes the vector of ridit scores corresponding to a vector of counts or proportions.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
to.ridit(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
  vector of counts or proportions
%%     ~~Describe \code{v} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
vector of ridit scores
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
function (v)
{
    (cumsum(v) - 0.5 * v)/sum(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
