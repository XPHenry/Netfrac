\name{multicore}
\alias{multicore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{multicore
%%  ~~function to do ... ~~
}
\description{Use parallel cores for bigger calculations. On Windows, the parallel mode might not work
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multicore(nc = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{number of cores
%%     ~~Describe \code{nc} here~~
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
function (nc = 0)
{
    cores <- if (.Platform$OS.type == "windows")
        1
    else min(8L, ceiling(detectCores()/2))
    getOption("mc.cores", cores)
    if (nc != 0)
        return(nc)
    return(cores)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
