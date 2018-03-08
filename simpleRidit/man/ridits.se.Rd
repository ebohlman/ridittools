\name{ridits.se}
\alias{ridits.se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute standard errors of mean ridits of multiple groups
%%  ~~function to do ... ~~
}
\description{
Takes the same data as ridits(), bui returns standard errors rather than means.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ridits.se(x, margin, ref = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  same as for ridits(), but must be counts rather than proportions
%%     ~~Describe \code{x} here~~
}
  \item{margin}{
  same as for ridits()
%%     ~~Describe \code{margin} here~~
}
  \item{ref}{
  same as for ridits()
%%     ~~Describe \code{ref} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of standard errors for each group's mean ridits
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
ridits
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
(ridits(semiauto, 2, 1) - 0.5) / ridits.se(semiauto, 2, 1)

## The function is currently defined as
function (x, margin, ref = NULL)
{
    apply(x, margin, se.ridit, ridits.refgroup(x, margin, ref))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
