\name{flu.age}
\alias{flu.age}
\docType{data}
\title{
Flu subtype by age group
%%   ~~ data name/kind ... ~~
}
\description{
Cross-tabulation of influenza virus subtypes by age group
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("flu.age")}
\format{
  The format is:
 num [1:4, 1:5] 274 1504 1665 1476 91 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:4] "0-4" "5-24" "25-64" "65+"
  ..$ : chr [1:5] "BY" "BV" "BU" "H3" ...
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://cdc.gov/flu/weekly for week ending 24 Feb 2018
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(flu.age)
}
\keyword{datasets}
