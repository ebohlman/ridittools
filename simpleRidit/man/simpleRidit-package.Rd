\name{simpleRidit-package}
\alias{simpleRidit-package}
\alias{simpleRidit}
\docType{package}
\title{
\packageTitle{simpleRidit}
}
\description{
\packageDescription{simpleRidit}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{simpleRidit}
\packageIndices{simpleRidit}
}
\author{
\packageAuthor{simpleRidit}

Maintainer: \packageMaintainer{simpleRidit}
}
\references{
Fleiss, J.,L., (1981), Statistical Methods for Rates and Proportions. New York: John Wiley & Sons.
}
\keyword{ package }
\seealso{
}
\examples{
ref <- acc[ , 1]
toridit(ref)
g <- acc[ , 2]
meanridit(g, ref)
seridit(g, ref)
ridits(flu.age, 2, "H3")
ridits(flu.age, 2) # Uses group totals as reference
riditsse(handgun, 2, 1)
}
