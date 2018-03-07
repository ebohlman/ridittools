\name{mean.ridit}
\alias{mean.ridit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute mean ridit of group given reference group
%%  ~~function to do ... ~~
}
\description{
Compute mean ridit for a group given a reference group
}
\usage{
mean.ridit(v, ref)
}
\arguments{
  \item{v}{
Vector of counts or proportions
}
  \item{ref}{
Vector of counts or propo0rtions to use as reference group
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The group's mean ridit
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Eric Bohlman
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
    sum(to.ridit(ref) * v)/sum(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
