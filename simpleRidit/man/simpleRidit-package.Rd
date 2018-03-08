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
to.ridit(ref)
g <- acc[ , 2]
meanridit(g, ref)
se.ridit(g, ref)
ridits(flu.age, 2, "H3")
ridits(flu.age, 2) # Uses group totals as reference
ridits.se(handgun, 2, 1)
}
